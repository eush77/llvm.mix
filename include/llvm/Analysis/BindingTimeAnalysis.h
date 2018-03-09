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

#include "llvm/ADT/DenseMap.h"
#include "llvm/Pass.h"

namespace llvm {

class Function;
class Instruction;
class raw_ostream;

class BindingTimeAnalysis : public FunctionPass {
  enum BindingTime {
    Static,
    Dynamic,
  };

public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

  // True if the instruction has static return binding time.
  bool isStatic(const Instruction *I) const;

  using FunctionPass::print;
  void print(raw_ostream &OS, const Function &F) const;

private:
  DenseMap<Instruction *, BindingTime> BindingTimes;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
