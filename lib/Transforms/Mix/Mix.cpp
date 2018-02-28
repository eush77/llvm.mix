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

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <iterator>

using namespace llvm;

#define DEBUG_TYPE "mix"

namespace {

class Mix : public ModulePass {
public:
  static char ID;

  Mix() : ModulePass(ID) {}

  bool runOnModule(Module &M) override;

private:
  // Resolve a metadata function id.
  Function *resolveFunctionId(MetadataAsValue *FID) const {
    return M->getFunction(cast<MDString>(FID->getMetadata())->getString());
  }

  // Create a specializer function.
  Function *createSpecializer(Function *F);

  Module *M;
};

char Mix::ID;

} // namespace

bool Mix::runOnModule(Module &M) {
  DEBUG(dbgs() << "---- Mix : " << M.getName() << " ----\n\n");

  this->M = &M;

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

        SmallVector<Value *, 8> Args;
        Args.push_back(Intr->getArgOperand(0));
        Args.append(std::next(std::next(Intr->arg_begin())), Intr->arg_end());

        auto *SpecializedFunction =
            CallInst::Create(Specializer, Args, "", Intr);
        SpecializedFunction->takeName(Intr);

        Intr->replaceAllUsesWith(SpecializedFunction);
        BBI = Intr->eraseFromParent();
      }
    }
  }

  DEBUG(dbgs() << '\n');
  return false;
}

Function *Mix::createSpecializer(Function *F) {
  DEBUG(dbgs() << "Creating specializer for @" << F->getName() << '\n');

  FunctionType *FT = F->getFunctionType();
  FunctionType *SpecFT;

  // Create function type of the specializer.
  {
    SmallVector<Type *, 8> Params;
    Params.push_back(Type::getInt8PtrTy(M->getContext()));
    Params.append(FT->param_begin(), FT->param_end());

    SpecFT = FunctionType::get(Type::getInt8PtrTy(M->getContext()), Params,
                               FT->isVarArg());
  }

  // Create specializer function.
  auto *SpecF = Function::Create(SpecFT, GlobalValue::PrivateLinkage,
                                 Twine(F->getName()) + ".mix");

  // Name parameters of the specializer.
  SpecF->arg_begin()->setName("context");
  for (auto ArgIt = F->arg_begin(), SpecArgIt = SpecF->arg_begin() + 1;
       ArgIt != F->arg_end(); ++ArgIt, ++SpecArgIt) {
    SpecArgIt->setName(ArgIt->getName());
  }

  M->getFunctionList().insertAfter(Module::iterator(F), SpecF);

  // Create instruction builders.
  IRBuilder<> Builder(BasicBlock::Create(M->getContext(), "", SpecF));
  StagedIRBuilder<decltype(Builder)> StagedBuilder(Builder, SpecF->arg_begin());

  // Create definitions for code generator in the entry block.
  auto *StagedModule =
      StagedBuilder.createModule(Twine(F->getName()) + ".module", "module");
  auto *StagedFunction = StagedBuilder.createFunction(
      FunctionType::get(FT->getReturnType(), false),
      GlobalValue::ExternalLinkage, F->getName(), StagedModule, "function");
  auto *StagedEntryBlock =
      StagedBuilder.createBasicBlock("", StagedFunction, "entry");
  StagedBuilder.createBuilder("builder");
  StagedBuilder.positionBuilderAtEnd(StagedEntryBlock);

  // Generate code.
  for (auto &BB : *F) {
    for (auto &I: BB) {
      StagedBuilder.createInstruction(&I);
    }
  }

  StagedBuilder.disposeBuilder();
  Builder.CreateRet(StagedModule);
  return SpecF;
}

static RegisterPass<Mix>
    RegisterMixPass("mix", "Compile staged functions for specialization");

void llvm::addMixPass(PassManagerBuilder &Builder) {
  for (auto EP : {PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast}) {
    Builder.addExtension(EP, [](const auto &, auto &PM) { PM.add(new Mix); });
  }
}
