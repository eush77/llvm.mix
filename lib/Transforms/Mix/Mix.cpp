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
#include "llvm/IR/Instruction.h"
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

#include <algorithm>
#include <cassert>
#include <iterator>
#include <memory>
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
  Function *createSpecializer(Function *F);
  static FunctionType *getSpecializerType(FunctionType *FT);
  void buildSpecializer();
  void buildBasicBlock(BasicBlock *BB, BasicBlock *SBB);
  void collectDynamicBlocks(BasicBlock *BB,
                            SetVector<BasicBlock *> &DynBBs) const;
  void buildInstruction(Instruction *I) const;

  Module *M{};
  Function *F{};
  const BindingTimeAnalysis *BTA{};
  Function *SF{};               // Specializer function.
  std::unique_ptr<IRBuilder<>> B;
  std::unique_ptr<mix::StagedIRBuilder<IRBuilder<>>> SB;

  // A mapping from basic blocks of the original function to static basic
  // blocks of the specializer.
  MapVector<BasicBlock *, BasicBlock *> StaticBasicBlocks;
};

char Mix::ID;

// Resolve a function by metadata id.
Function *resolveFunctionId(const Module &M, MetadataAsValue *FID) {
  return M.getFunction(cast<MDString>(FID->getMetadata())->getString());
}

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

        auto *Specializer = createSpecializer(resolveFunctionId(
            M, cast<MetadataAsValue>(Intr->getArgOperand(1))));
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

Function *Mix::createSpecializer(Function *F) {
  this->F = F;
  BTA = &getAnalysis<BindingTimeAnalysis>(*F);

  DEBUG(dbgs() << "Creating specializer for @" << F->getName() << '\n');

  // Create specializer function.
  SF = Function::Create(getSpecializerType(F->getFunctionType()),
                        GlobalValue::PrivateLinkage,
                        Twine(F->getName()) + ".mix");

  // Name parameters of the specializer.
  SF->arg_begin()->setName("context");
  for (auto ArgIt = F->arg_begin(), SpecArgIt = SF->arg_begin() + 1;
       ArgIt != F->arg_end(); ++ArgIt, ++SpecArgIt) {
    SpecArgIt->setName(ArgIt->getName());
  }

  // Insert specializer function into the module.
  M->getFunctionList().insertAfter(Module::iterator(F), SF);

  buildSpecializer();
  return SF;
}

FunctionType *Mix::getSpecializerType(FunctionType *FT) {
  SmallVector<Type *, 8> Params;

  Params.push_back(mix::getContextPtrTy(FT->getContext()));
  Params.append(FT->param_begin(), FT->param_end());

  return FunctionType::get(mix::getModulePtrTy(FT->getContext()), Params,
                           FT->isVarArg());
}

// Build code of the specializer function.
void Mix::buildSpecializer() {
  B.reset(new IRBuilder<>(
      BasicBlock::Create(SF->getContext(), F->getEntryBlock().getName(), SF)));
  SB.reset(new mix::StagedIRBuilder<IRBuilder<>>(*B, SF->arg_begin()));

  // Build basic definitions for run-time code generator.
  auto *StagedModule = SB->createModule(Twine(F->getName()) + ".module", "module");
  auto *StagedFunction = SB->createFunction(
      FunctionType::get(F->getReturnType(), false),
      GlobalValue::ExternalLinkage, F->getName(), StagedModule, "function");
  SB->createBuilder(StagedFunction, "builder");

  StaticBasicBlocks.clear();
  StaticBasicBlocks[&F->getEntryBlock()] = &SF->getEntryBlock();

  for (unsigned SBBNum = 0; SBBNum < StaticBasicBlocks.size(); ++SBBNum) {
    BasicBlock *BB;
    BasicBlock *SBB;
    std::tie(BB, SBB) = StaticBasicBlocks.begin()[SBBNum];

    buildBasicBlock(BB, SBB);
  }

  SB->disposeBuilder();
  B->CreateRet(StagedModule);
}

void Mix::buildBasicBlock(BasicBlock *BB, BasicBlock *SBB) {
  DEBUG(dbgs() << "  - Building static basic block %" << BB->getName() << '\n');

  B->SetInsertPoint(SBB);

  SetVector<BasicBlock *> DynBBs;
  collectDynamicBlocks(BB, DynBBs);

  for (auto *DynBB : DynBBs) {
    SB->positionBuilderAtEnd(SB->stage(DynBB));

    std::for_each(DynBB->begin(), DynBB->end(),
                  [this](Instruction &I) { buildInstruction(&I); });
  }

  BasicBlock *TermBB = DynBBs.back();

  // Create new successor static blocks.
  if (BTA->getBindingTime(TermBB->getTerminator()) ==
      BindingTimeAnalysis::Static) {
    for (auto *SuccBB : successors(TermBB)) {
      if (!StaticBasicBlocks.count(SuccBB)) {
        StaticBasicBlocks[SuccBB] =
            BasicBlock::Create(SF->getContext(), SuccBB->getName(), SF);
      }
    }
  }
}

// Traverse dynamic blocks in the CFG starting from the given static block.
//
// The static block and all of dynamic blocks are added to the vector. The
// last block added is the one ending with the static terminator for the
// series.
void Mix::collectDynamicBlocks(BasicBlock *BB,
                               SetVector<BasicBlock *> &DynBBs) const {
  assert(BTA->getBindingTime(BB) == BindingTimeAnalysis::Static);

  // If DynBBs already contains the starting block, move it to the end, so
  // that we can start iteration at the of the collection.
  if (!DynBBs.insert(BB)) {
    DynBBs.remove(BB);
    DynBBs.insert(BB);
  }

  // The block containing the static terminator of the original basic block.
  // This is added to DynBBs last.
  BasicBlock *TermBB = nullptr;

  for (unsigned BBNum = DynBBs.size() - 1; BBNum < DynBBs.size(); ++BBNum) {
    BasicBlock *DynBB = DynBBs[BBNum];
    TerminatorInst *Term = DynBB->getTerminator();

    // Check if static terminator is reached.
    if (BTA->getBindingTime(Term) == BindingTimeAnalysis::Static) {
      assert(!TermBB &&
             "Multiple static terminators reachable from a static block");
      TermBB = DynBB;
      continue;
    }

    for (BasicBlock *SuccBB : Term->successors()) {
      DynBBs.insert(SuccBB);
    }
  }

  if (TermBB) {
    DynBBs.insert(TermBB);
  }
}

void Mix::buildInstruction(Instruction *I) const {
  switch (BTA->getBindingTime(I)) {
  case BindingTimeAnalysis::Static:
    B->Insert(I);
    break;

  case BindingTimeAnalysis::Dynamic:
    SB->stage(I);
    break;
  }
}

INITIALIZE_PASS_BEGIN(Mix, "mix", "Multi-Stage Compilation", false, false)
INITIALIZE_PASS_DEPENDENCY(BindingTimeAnalysis)
INITIALIZE_PASS_END(Mix, "mix", "Multi-Stage Compilation", false, false)

void llvm::addMixPass(PassManagerBuilder &PMB) {
  for (auto EP : {PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast}) {
    PMB.addExtension(EP, [](const auto &, auto &PM) { PM.add(new Mix); });
  }
}
