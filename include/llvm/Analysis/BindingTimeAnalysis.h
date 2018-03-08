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

#include "llvm/Pass.h"

namespace llvm {

class Function;
class Instruction;
class raw_ostream;

class BindingTimeAnalysis : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

  bool isStatic(const Instruction *I) const { return false; }

  using FunctionPass::print;
  void print(raw_ostream &OS, const Function &F) const;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
