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
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/ModuleSlotTracker.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Printable.h"

#include <queue>
#include <utility>

namespace llvm {

class Argument;
class Function;
class Instruction;
class PHINode;
class raw_ostream;
class TerminatorInst;
class Value;

class BindingTimeAnalysis : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

  // Last compilation stage of the function.
  unsigned getLastStage() const { return LastStage; }

  // Get binding-time stage of a value.
  unsigned getStage(const Value *V) const;

  // Binding-time stage of PHINode always matches the stage of its basic
  // block. This function return the stage of its value.
  unsigned getPhiValueBindingTime(const PHINode *Phi) const;

  using FunctionPass::print;
  void print(raw_ostream &OS, const Function &F);

private:
  friend class BindingTimeAnalysisPlainAssemblyAnnotationWriter;
  friend class BindingTimeAnalysisColorAssemblyAnnotationWriter;

  bool isLastStage(const Instruction *I) const;
  bool updateStage(const Value *V, unsigned Stage);
  void addTransitiveNonPhiUsers(const Value *V, unsigned IncomingStage);

  void fixArgument(const Argument *A, unsigned Stage);
  void fixBasicBlock(const BasicBlock *BB, unsigned Stage);
  void fixInstruction(const Instruction *I, unsigned MaxOperandStage);

  void initializeWorklist();
  void fixTerminators();
  void fix();

  Printable printName(const Value *V);
  Printable printInstWithBlock(const Instruction *I, StringRef Prefix = "");

#ifndef NDEBUG
  Printable printValueWithKind(const Value *V);

  template <typename Reason>
  void dumpStageImpl(const Value *V, bool StageChange, unsigned Stage,
                     const Value *DumpV, Reason R);

  template <typename Reason>
  void dumpStage(const Value *V, unsigned Stage, Reason R);

  template <typename Reason>
  void dumpStageChange(const Value *V, unsigned NewStage, const Value *DumpV,
                       Reason R);
#endif

  class WorklistItem : std::pair<unsigned, const Value *> {
    using Base = std::pair<unsigned, const Value *>;

  public:
    WorklistItem(const Value *Value, unsigned IncomingStage)
        : Base(IncomingStage, Value) {}

    template <typename T> bool is() const { return isa<T>(second); }
    template <typename T> const T *get() const { return dyn_cast<T>(second); }
    unsigned getIncomingStage() const { return first; }

    // Compare by incoming stage.
    bool operator<(const WorklistItem &Other) const {
      return static_cast<const Base &>(*this) < Other;
    }
  };

  // Work list prioritizing higher-stage values over lower-stage values.
  std::priority_queue<WorklistItem> Worklist;

  // Mapping of values to binding-time stages.
  DenseMap<const Value *, unsigned> StageMap;

  // True if `getStage' returns 0 for unanalyzed values. Used during analysis.
  bool DefaultToStage0 = false;

  // Current function.
  Function *F{};

  // Last stage for the current function.
  unsigned LastStage{};

  // Slot tracker for printing.
  Optional<ModuleSlotTracker> MST;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
