//===- Mix.cpp ------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the runtime IR specializer pass.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Mix.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

#define DEBUG_TYPE "mix"


namespace {

class Mix : public ModulePass {
public:
  static char ID;

  Mix(): ModulePass(ID) {}

  bool runOnModule(Module &M) override;
};

char Mix::ID;

} // end of anonymous namespace

bool Mix::runOnModule(Module &M) {
  for (auto &F: M) {
    DEBUG(dbgs() << "Function @" << F.getName() << '\n');
  }
  return false;
}

static RegisterPass<Mix> RegisterMixPass(
  "mix",
  "Compile staged functions for runtime specialization");

void llvm::addMixPass(PassManagerBuilder &Builder) {
  for (auto EP: { PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast }) {
    Builder.addExtension(EP, [](const auto &Builder, auto &PM) {
      PM.add(new Mix);
    });
  }
}
