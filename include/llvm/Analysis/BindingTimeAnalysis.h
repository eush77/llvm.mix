//===- BindingTimeAnalysis.h - Binding Time Analysis ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass is used to compute binding-time division for instructions in a
// function.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
#define LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H

#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"

namespace llvm {

class Function;
class Instruction;

class BindingTimeAnalysis : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {
    initializeBindingTimeAnalysisPass(*PassRegistry::getPassRegistry());
  }

  bool runOnFunction(Function &F) override;

  bool isStatic(const Instruction *I) const { return false; }
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
