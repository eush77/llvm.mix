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
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/ModuleSlotTracker.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Printable.h"

#include <queue>
#include <string>

namespace llvm {

class Argument;
class DiagnosticPrinter;
class Function;
class Instruction;
class IntrinsicInst;
class Module;
class PHINode;
class raw_ostream;
class TerminatorInst;
class Twine;
class Use;
class Value;

// Returns the intrinsic call that annotates the value with an object stage,
// or null if no such call exists.
//
// See documentation for `@llvm.object.stage` intrinsic.
const IntrinsicInst *getObjectStageAnnotation(const Value *);

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

  // Print results for last analyzed function.
  void print(raw_ostream &OS, const Module *M) const override;

private:
  friend class BindingTimeAnalysisPlainAssemblyAnnotationWriter;
  friend class BindingTimeAnalysisColorAssemblyAnnotationWriter;

  unsigned getObjectStage(const Value *V) const;
  bool isLastStage(const Instruction *I) const;
  bool updateStage(const Value *V, unsigned Stage);
  void verifyUse(const Use &U, unsigned IncomingStage);
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
      CallStage,
      Default,
      LastStage,
      ObjectStage,
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
             (InStage == Other.InStage && ID > Other.ID);
    }

    void print(raw_ostream &OS, BindingTimeAnalysis &BTA) const;
    LLVM_DUMP_METHOD void dump() const;

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
  LLVM_DUMP_METHOD void dumpWorklist() const;

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

class DiagnosticInfoBindingTime : public DiagnosticInfoWithLocationBase {
public:
  static bool classof(const DiagnosticInfo *DI) {
    return DI->getKind() == DK_BindingTime;
  }

  // Construct diagnosic from a format string, IR value, and optionally a
  // stage number, with the first `%s' in the format string refering to the
  // string representation of the IR value, and the second `%s' refering to
  // the string representation of the stage.
  DiagnosticInfoBindingTime(DiagnosticSeverity Severity, const Function &F,
                            const Twine &Fmt, const Value *V,
                            Optional<unsigned> Stage = None);

  const std::string &getMessage() const { return Msg; }
  const Value *getValue() const { return V; }
  const Optional<unsigned> &getStage() const { return Stage; }

  void print(DiagnosticPrinter &DP) const override;

private:
  std::string Msg;
  const Value *V;
  Optional<unsigned> Stage;
};

} // end namespace llvm

#endif // LLVM_ANALYSIS_BINDINGTIMEANALYSIS_H
