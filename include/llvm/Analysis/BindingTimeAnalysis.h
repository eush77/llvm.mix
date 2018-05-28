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
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Pass.h"

#include <iterator>

namespace llvm {

class Function;
class Instruction;
class raw_ostream;
class TerminatorInst;

class BindingTimeAnalysis : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

  enum BindingTime {
    Static,
    Dynamic,
  };

  // Get binding time of instruction or basic block.
  BindingTime getBindingTime(const BasicBlock *BB) const;
  BindingTime getBindingTime(const Instruction *I) const;

  // Get static terminator of a basic block, or null.
  const TerminatorInst *getStaticTerminator(const BasicBlock *BB) const {
    return StaticTerminators.lookup(BB);
  }

  using FunctionPass::print;
  void print(raw_ostream &OS, const Function &F) const;

private:
  void initializeBindingTimeDivision(const Function &F);
  void computeBindingTimeDivision(const Function &F);
  void computeStaticTerminators(const Function &F);

  DenseMap<const BasicBlock *, BindingTime> BasicBlockBindingTimes;
  DenseMap<const Instruction *, BindingTime> InstructionBindingTimes;
  DenseMap<const BasicBlock *, const TerminatorInst *> StaticTerminators;

  // A queue of marked instructions and next position in the queue.
  //
  // Each instruction added here must have a dynamic value that will be
  // propagated to all users. Note that this description includes static phis
  // with dynamic operands as well as dynamic non-phis.
  SetVector<const Instruction *> MarkedInstructions;
  unsigned NextMarkedInstructionNumber = 0;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
