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
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/ModuleSlotTracker.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Printable.h"

#include <queue>

namespace llvm {

class Argument;
class DiagnosticPrinter;
class Function;
class Instruction;
class PHINode;
class raw_ostream;
class TerminatorInst;
class Twine;
class Value;

class BindingTimeAnalysis : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysis() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

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

  bool isAnalyzed(const Value *V) const;

  Printable printName(const Value *V);
  Printable printValueWithKind(const Value *V);
  Printable printInstWithBlock(const Instruction *I, StringRef Prefix = "");

  class WorklistItem {
  public:
    enum Reason {
      Attribute,
      Default,
      LastStage,
      Operand,
      Parent,
      PredTerminator,
      ReturnStage,
      StageTerminator,
      Successor,
      TransitiveOperand,
    };

    WorklistItem(unsigned ID, const Value *V, unsigned IncomingStage, Reason R,
                 const Value *Sender = nullptr, const Value *Arg = nullptr)
        : ID(ID), V(V), InStage(IncomingStage), R(R), Sender(Sender), Arg(Arg) {
    }

    template <typename T> const T *get() const { return dyn_cast<T>(V); }
    unsigned getIncomingStage() const { return InStage; }

    // Comparison operator for std::priority_queue.
    //
    // If incoming stages differ, prioritize the item with the later stage.
    // If incoming stages are equal, prioritize the item that was created
    // earlier.
    bool operator<(const WorklistItem &Other) const {
      return (InStage < Other.InStage) ||
             (InStage == Other.InStage && ID > Other.InStage);
    }

    void print(raw_ostream &OS, BindingTimeAnalysis &BTA) const;

  private:
    unsigned ID;
    const Value *V;
    unsigned InStage;
    Reason R;
    const Value *Sender;
    const Value *Arg;
  };

  template <typename... ArgsT>
  void enqueue(const Value *V, unsigned IncomingStage, WorklistItem::Reason R,
               ArgsT... Args);
  Optional<WorklistItem> popItem();

  // Work list prioritizing higher-stage values over lower-stage values.
  std::priority_queue<WorklistItem> Worklist;
  unsigned WorklistID{};

  // Mapping of values to binding-time stages.
  DenseMap<const Value *, unsigned> StageMap;

  // True if `getStage' returns 0 for unanalyzed values. Used during analysis.
  bool DefaultToStage0 = false;

  // Current function.
  Function *F{};

  // Slot tracker for printing.
  Optional<ModuleSlotTracker> MST;
};

class DiagnosticInfoMix : public DiagnosticInfo {
public:
  static bool classof(const DiagnosticInfo *DI) {
    return DI->getKind() == DK_Mix;
  }

  DiagnosticInfoMix(DiagnosticSeverity Severity, const Twine &Msg,
                    const Instruction *I = nullptr)
      : DiagnosticInfo(DK_Mix, Severity), Msg(Msg), I(I) {}

  const Twine &getMessage() const { return Msg; }
  const Instruction *getInstruction() const { return I; }

  void print(DiagnosticPrinter &DP) const override;

private:
  const Twine &Msg;
  const Instruction *I;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
