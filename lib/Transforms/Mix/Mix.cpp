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

char MixPassID;

class MixPass : public ModulePass {
public:
  MixPass(): ModulePass(MixPassID) {}

  bool runOnModule(Module &M) override;
};

} // end of anonymous namespace

bool MixPass::runOnModule(Module &M) {
  for (auto &F: M) {
    DEBUG(dbgs() << "Function @" << F.getName() << '\n');
  }
  return false;
}

void llvm::addMixPass(PassManagerBuilder &Builder) {
  for (auto EP: { PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast }) {
    Builder.addExtension(EP, [](const auto &Builder, auto &PM) {
      PM.add(new MixPass);
    });
  }
}
