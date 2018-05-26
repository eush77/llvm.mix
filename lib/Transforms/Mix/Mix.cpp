//===- Mix.cpp ------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the IR specializer pass.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Mix.h"
#include "StagedIRBuilder.h"
#include "Types.h"

#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Analysis/BindingTimeAnalysis.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <cassert>
#include <iterator>
#include <tuple>

using namespace llvm;

#define DEBUG_TYPE "mix"

namespace {

class Mix : public ModulePass {
public:
  static char ID;

  Mix() : ModulePass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BindingTimeAnalysis>();
  }

  bool runOnModule(Module &M) override;

private:
  // Resolve a metadata function id.
  Function *resolveFunctionId(MetadataAsValue *FID) const {
    return M->getFunction(cast<MDString>(FID->getMetadata())->getString());
  }

  Function *createSpecializer(Function *F);
  void buildSpecializer(Function *F, Function *SpecF) const;
  void collectDynamicBlocks(BasicBlock *SBB,
                            SetVector<BasicBlock *> &Blocks) const;

  Module *M = nullptr;
  const BindingTimeAnalysis *BTA = nullptr;
};

char Mix::ID;

} // namespace

bool Mix::runOnModule(Module &M) {
  DEBUG(dbgs() << "---- Mix : " << M.getName() << " ----\n\n");

  this->M = &M;
  bool Changed = false;

  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto BBI = BB.begin(); BBI != BB.end();) {
        if (!isa<IntrinsicInst>(BBI)) {
          ++BBI;
          continue;
        }

        auto *Intr = cast<IntrinsicInst>(BBI);

        if (Intr->getIntrinsicID() != Intrinsic::mix) {
          ++BBI;
          continue;
        }

        auto *Specializer = createSpecializer(
            resolveFunctionId(cast<MetadataAsValue>(Intr->getArgOperand(1))));
        auto *StagedContext = new BitCastInst(
            Intr->getArgOperand(0), mix::getContextPtrTy(M.getContext()),
            Intr->getArgOperand(0)->getName(), Intr);

        SmallVector<Value *, 8> Args;
        Args.push_back(StagedContext);
        Args.append(std::next(std::next(Intr->arg_begin())), Intr->arg_end());

        auto *StagedModule =
            new BitCastInst(CallInst::Create(Specializer, Args, "", Intr),
                            Intr->getType(), "", Intr);
        StagedModule->takeName(Intr);
        Intr->replaceAllUsesWith(StagedModule);
        BBI = Intr->eraseFromParent();

        Changed = true;
      }
    }
  }

  return Changed;
}

namespace {

FunctionType *getSpecializerType(FunctionType *FT) {
  SmallVector<Type *, 8> Params;

  Params.push_back(mix::getContextPtrTy(FT->getContext()));
  Params.append(FT->param_begin(), FT->param_end());

  return FunctionType::get(mix::getModulePtrTy(FT->getContext()), Params,
                           FT->isVarArg());
}

} // namespace

Function *Mix::createSpecializer(Function *F) {
  BTA = &getAnalysis<BindingTimeAnalysis>(*F);

  DEBUG(dbgs() << "Creating specializer for @" << F->getName() << '\n');

  // Create specializer function.
  auto *SpecF = Function::Create(getSpecializerType(F->getFunctionType()),
                                 GlobalValue::PrivateLinkage,
                                 Twine(F->getName()) + ".mix");

  // Name parameters of the specializer.
  SpecF->arg_begin()->setName("context");
  for (auto ArgIt = F->arg_begin(), SpecArgIt = SpecF->arg_begin() + 1;
       ArgIt != F->arg_end(); ++ArgIt, ++SpecArgIt) {
    SpecArgIt->setName(ArgIt->getName());
  }

  // Insert specializer function into the module.
  M->getFunctionList().insertAfter(Module::iterator(F), SpecF);

  buildSpecializer(F, SpecF);
  return SpecF;
}

// Build code of the specializer function SpecC given the original function F.
void Mix::buildSpecializer(Function *F, Function *SpecF) const {
  IRBuilder<> Builder(BasicBlock::Create(SpecF->getContext(),
                                         F->getEntryBlock().getName(), SpecF));
  mix::StagedIRBuilder<decltype(Builder)> StagedBuilder(Builder,
                                                        SpecF->arg_begin());

  // Build basic definitions for run-time code generator.
  auto *StagedModule =
      StagedBuilder.createModule(Twine(F->getName()) + ".module", "module");
  auto *StagedFunction = StagedBuilder.createFunction(
      FunctionType::get(F->getReturnType(), false),
      GlobalValue::ExternalLinkage, F->getName(), StagedModule, "function");
  StagedBuilder.createBuilder(StagedFunction, "builder");

  // A mapping from basic blocks of the original function to static basic
  // blocks of the specializer.
  MapVector<BasicBlock *, BasicBlock *> StaticBasicBlocks;
  StaticBasicBlocks[&F->getEntryBlock()] = &SpecF->getEntryBlock();

  for (unsigned SBBNum = 0; SBBNum < StaticBasicBlocks.size(); ++SBBNum) {
    BasicBlock *SBB;
    BasicBlock *SpecSBB;
    std::tie(SBB, SpecSBB) = StaticBasicBlocks.begin()[SBBNum];

    DEBUG(dbgs() << "  - Building static basic block %" << SBB->getName()
          << '\n');

    Builder.SetInsertPoint(SpecSBB);

    SetVector<BasicBlock *> BBs;
    collectDynamicBlocks(SBB, BBs);

    // Construct blocks in sequence.
    for (auto *BB : BBs) {
      StagedBuilder.positionBuilderAtEnd(StagedBuilder.stage(BB));

      for (auto &I: *BB) {
        switch (BTA->getBindingTime(&I)) {
        case BindingTimeAnalysis::Static:
          Builder.Insert(&I);
          break;

        case BindingTimeAnalysis::Dynamic:
          StagedBuilder.stage(&I);
          break;
        }
      }
    }

    if (BTA->getBindingTime(BBs.back()->getTerminator()) ==
        BindingTimeAnalysis::Static) {
      for (auto *SuccBB : successors(BBs.back())) {
        if (!StaticBasicBlocks.count(SuccBB)) {
          StaticBasicBlocks[SuccBB] =
              BasicBlock::Create(SpecF->getContext(), SuccBB->getName(), SpecF);
        }
      }
    }
  }

  StagedBuilder.disposeBuilder();
  Builder.CreateRet(StagedModule);
}

// Traverse dynamic blocks in the CFG starting from the given static block.
//
// The static block and all of dynamic blocks are added to the vector. The
// last block added is the one ending with the static terminator for the
// series.
void Mix::collectDynamicBlocks(BasicBlock *SBB,
                               SetVector<BasicBlock *> &Blocks) const {
  assert(BTA->getBindingTime(SBB) == BindingTimeAnalysis::Static);

  // If Blocks already contains SBB, move SBB to the end, so that we can start
  // iteration at the last element assuming it is indeed SBB.
  if (!Blocks.insert(SBB)) {
    Blocks.remove(SBB);
    Blocks.insert(SBB);
  }

  // Terminator block is set when discovered, and then pushed last.
  BasicBlock *TerminatorBB = nullptr;

  for (unsigned BBNum = Blocks.size() - 1; BBNum < Blocks.size(); ++BBNum) {
    BasicBlock *BB = Blocks[BBNum];
    TerminatorInst *Term = BB->getTerminator();

    // Check if static terminator is reached.
    if (BTA->getBindingTime(Term) == BindingTimeAnalysis::Static) {
      assert(!TerminatorBB &&
             "Multiple static terminators reachable from a static block");
      TerminatorBB = BB;
      continue;
    }

    for (BasicBlock *SuccBB : Term->successors()) {
      Blocks.insert(SuccBB);
    }
  }

  if (TerminatorBB) {
    Blocks.insert(TerminatorBB);
  }
}

INITIALIZE_PASS_BEGIN(Mix, "mix", "Multi-Stage Compilation", false, false)
INITIALIZE_PASS_DEPENDENCY(BindingTimeAnalysis)
INITIALIZE_PASS_END(Mix, "mix", "Multi-Stage Compilation", false, false)

void llvm::addMixPass(PassManagerBuilder &Builder) {
  for (auto EP : {PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast}) {
    Builder.addExtension(EP, [](const auto &, auto &PM) { PM.add(new Mix); });
  }
}
