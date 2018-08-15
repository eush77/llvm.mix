//===- BindingTimeAnalysis.cpp - Binding Time Analysis --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements binding time analysis.
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/BindingTimeAnalysis.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Printable.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"

#include <algorithm>
#include <cassert>
#include <iterator>
#include <memory>
#include <numeric>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "bta"

// True if the instruction must be evaluated at the last stage.
bool BindingTimeAnalysis::isLastStage(const Instruction *I) const {
  if (I->mayHaveSideEffects() || I->mayReadFromMemory()) {
    return true;
  }

  switch (I->getOpcode()) {
  case Instruction::Br:
  case Instruction::IndirectBr:
  case Instruction::Switch:
    return false;

  case Instruction::Alloca:
  case Instruction::Call:
  case Instruction::Invoke:
    return true;

  default:
    return I->getType()->isVoidTy();
  }
}

// Update the stage of value, returning true if the update changed the current
// stage, false otherwise.
bool BindingTimeAnalysis::updateStage(const Value *V, unsigned Stage) {
  unsigned &CurrentStage = StageMap[V];

  if (Stage <= CurrentStage)
    return false;

  CurrentStage = Stage;
  return true;
}

// Add transitive non-phi users of a value to the worklist with a given
// incoming stage value.
void BindingTimeAnalysis::addTransitiveNonPhiUsers(const Value *V,
                                                   unsigned IncomingStage) {
  SmallSetVector<const Value *, 4> TransitiveUsers(V->user_begin(),
                                                   V->user_end());

  for (unsigned TUNum = 0; TUNum < TransitiveUsers.size(); ++TUNum) {
    const Value *TU = TransitiveUsers[TUNum];

    // Close the set of transitive users by including users of phi users.
    if (auto *Phi = dyn_cast<PHINode>(TU)) {
      for (const Value *V : Phi->users()) {
        TransitiveUsers.insert(V);
      }
    } else {
      DEBUG(dumpStageChange(TU, IncomingStage, TU,
                            Printable([&](raw_ostream &OS) {
                              OS << "user of " << printName(V);
                            })));
      Worklist.emplace(TU, IncomingStage);
    }
  }
}

void BindingTimeAnalysis::fixArgument(const Argument *A, unsigned Stage) {
  if (updateStage(A, Stage)) {
    addTransitiveNonPhiUsers(A, Stage);
  }
}

void BindingTimeAnalysis::fixBasicBlock(const BasicBlock *BB, unsigned Stage) {
  if (!updateStage(BB, Stage))
    return;

  for (auto *PredBB : predecessors(BB)) {
    const TerminatorInst *PredTerm = PredBB->getTerminator();

    DEBUG(dumpStageChange(PredTerm, Stage, PredTerm,
                          Printable([&](raw_ostream &OS) {
                            OS << "branching to " << printName(BB);
                          })));
    Worklist.emplace(PredTerm, Stage);
  }

  for (const PHINode &Phi : BB->phis()) {
    DEBUG(dumpStageChange(&Phi, Stage, &Phi, Printable([&](raw_ostream &OS) {
      OS << "phi in block " << printName(BB);
    })));
    Worklist.emplace(&Phi, Stage);
  }
}

void BindingTimeAnalysis::fixInstruction(const Instruction *I,
                                         unsigned MaxOperandStage) {
  if (!updateStage(I, MaxOperandStage))
    return;

  addTransitiveNonPhiUsers(I, MaxOperandStage);

  if (auto *Term = dyn_cast<TerminatorInst>(I)) {
    for (const BasicBlock *BB : Term->successors()) {
      DEBUG(dumpStageChange(
          BB, MaxOperandStage, Term, Printable([&](raw_ostream &OS) {
            OS << "destination of terminator " << printName(Term);
          })));
      Worklist.emplace(BB, MaxOperandStage);
    }
  }
}

void BindingTimeAnalysis::initializeWorklist() {
  assert(Worklist.empty() && "Unfinished analysis");

  // Add function arguments to the worklist.
  for (const Argument &A : F->args()) {
    DEBUG(dumpStage(&A, A.getStage(), "stage attribute"));
    Worklist.emplace(&A, A.getStage());
  }

  // Add basic blocks and instructions to the worklist.
  for (const BasicBlock &BB : *F) {
    DEBUG(dumpStage(&BB, 0, "default"));
    Worklist.emplace(&BB, 0);

    for (const Instruction &I : BB) {
      if (isLastStage(&I)) {
        DEBUG(dumpStage(&I, LastStage, "last stage"));
        Worklist.emplace(&I, LastStage);
      } else {
        DEBUG(dumpStage(&I, 0, "default"));
        Worklist.emplace(&I, 0);
      }
    }
  }
}

// At each stage, every basic block has to have at most one terminator.
void BindingTimeAnalysis::fixTerminators() {
  for (auto &SourceBB : *F) {
    for (unsigned Stage = getStage(&SourceBB); Stage <= LastStage; ++Stage) {
      const TerminatorInst *Term = nullptr;

      for (auto BBIter = df_begin(&SourceBB); BBIter != df_end(&SourceBB);) {
        const TerminatorInst *T = (*BBIter)->getTerminator();

        if (Stage < getStage(T)) {
          ++BBIter;
          continue;
        }

        BBIter.skipChildren();

        if (!Term) {
          Term = T;
          continue;
        }

        assert(T != Term);

        // Favor lower-stage terminators.
        if (getStage(T) < getStage(Term)) {
          std::swap(Term, T);
        }

        errs() << "Multiple terminators for " << printName(&SourceBB)
               << " at stage " << Stage << ":\n"
               << printInstWithBlock(Term, "  (         )")
               << printInstWithBlock(T, "  (->dynamic)");

        Worklist.emplace(T, Stage + 1);
      }
    }
  }
}

// Compute a fixed point of binding-time rules.
void BindingTimeAnalysis::fix() {
  SaveAndRestore<bool> SavedDTS0(DefaultToStage0, true);

  do {
    while (!Worklist.empty()) {
      WorklistItem WI(Worklist.top());
      Worklist.pop();

      unsigned IS = WI.getIncomingStage();

      if (auto *A = WI.get<Argument>()) {
        fixArgument(A, IS);
      } else if (auto *BB = WI.get<BasicBlock>()) {
        fixBasicBlock(BB, IS);
      } else if (auto *I = WI.get<Instruction>()) {
        fixInstruction(I, IS);
      } else {
        llvm_unreachable("Unsupported value kind");
      }
    }

    fixTerminators();
  } while (!Worklist.empty());
}

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");

  this->F = &F;

  // Compute the last stage for this function from the stage numbers assigned
  // to the arguments.
  //
  // FIXME: It is reasonable to expect the last stage to be N as well as N+1
  // if N is the maximum stage of the arguments. It is reasonable to expect
  // function with no arguments and no dynamic effects to be in stage 0 as
  // well.
  LastStage = std::accumulate(F.arg_begin(), F.arg_end(), 1,
                              [](unsigned LS, const Argument &A) {
                                return std::max(LS, A.getStage() + 1);
                              });

  // Initialize slot tracker for printing.
  MST.emplace(F.getParent());
  MST->incorporateFunction(F);

  // Reset previous state.
  StageMap.clear();

  initializeWorklist();
  fix();
  return false;
}

unsigned BindingTimeAnalysis::getStage(const Value *V) const {
  if (isa<Constant>(V))
    return 0;

  auto Iter = StageMap.find(V);

  if (Iter == StageMap.end()) {
    assert(DefaultToStage0 && "Value has not been analyzed");
    return 0;
  }

  return Iter->second;
}

unsigned BindingTimeAnalysis::getPhiValueBindingTime(const PHINode *Phi) const {
  return std::accumulate(Phi->op_begin(), Phi->op_end(), 0,
                         [this](unsigned Stage, const Value *V) {
                           return std::max(Stage, getStage(V));
                         });
}

// Print name of a local value.
Printable BindingTimeAnalysis::printName(const Value *V) {
  assert(!isa<GlobalValue>(V) && "Not a local value");

  return Printable([=](raw_ostream &OS) {
    if (V->hasName()) {
      OS << '%';
      printLLVMNameWithoutPrefix(OS, V->getName());
      return;
    }

    int Slot = MST->getLocalSlot(V);

    if (Slot != -1) {
      OS << '%' << Slot;
    } else if (auto *I = dyn_cast<Instruction>(V)) {
      OS << I->getOpcodeName();
    } else {
      OS << "<badref>";
    }
  });
}

constexpr unsigned CommentColumn = 50;

// Print instruction and comment it with the name of the parent basic block.
Printable BindingTimeAnalysis::printInstWithBlock(const Instruction *I,
                                                  StringRef Prefix) {
  return Printable([=](raw_ostream &OS) {
    formatted_raw_ostream FOS(OS);

    FOS << Prefix;
    I->print(FOS, *MST);
    FOS.PadToColumn(CommentColumn) << "; " << printName(I->getParent()) << '\n';
  });
}

#ifndef NDEBUG
// Print name of a value preceded by a kind of value.
Printable BindingTimeAnalysis::printValueWithKind(const Value *V) {
  Printable Name = printName(V);

  if (isa<Instruction>(V)) {
    return Printable([=](raw_ostream &OS) { OS << "Instruction " << Name; });
  }

  const char *Kind = nullptr;

  switch (V->getValueID()) {
#define HANDLE_VALUE(Name)                                                     \
  case Value::Name##Val:                                                       \
    Kind = #Name;                                                              \
    break;
#include "llvm/IR/Value.def"
  default:
    break;
  }

  return Kind ? Printable([=](raw_ostream &OS) { OS << Kind << ' ' << Name; })
              : Name;
}

template <typename Reason>
void BindingTimeAnalysis::dumpStageImpl(const Value *V, bool StageChange,
                                        unsigned Stage, const Value *DumpV,
                                        Reason R) {
  dbgs() << printValueWithKind(V) << " stage ";

  if (StageChange) {
    dbgs() << getStage(V) << "->";
  }

  dbgs() << Stage << " (" << R << ')' << '\n';

  if (auto *I = dyn_cast<Instruction>(DumpV)) {
    dbgs() << printInstWithBlock(I);
  }
}

template <typename Reason>
void BindingTimeAnalysis::dumpStage(const Value *V, unsigned Stage, Reason R) {
  return dumpStageImpl(V, false, Stage, V, R);
}

template <typename Reason>
void BindingTimeAnalysis::dumpStageChange(const Value *V, unsigned NewStage,
                                          const Value *DumpV, Reason R) {
  return dumpStageImpl(V, true, NewStage, DumpV, R);
}
#endif

namespace llvm {

// Plain AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Prints the results in plain-text comments beneath basic blocks and to the
// right of instructions.
class BindingTimeAnalysisPlainAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  explicit BindingTimeAnalysisPlainAssemblyAnnotationWriter(
      BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    OS << printStage(BB) << '\n';
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    OS.PadToColumn(CommentColumn) << printStage(&V);
  }

protected:
  BindingTimeAnalysis &BTA;

private:
  Printable printStage(const Value *V) const {
    return Printable(
        [=](raw_ostream &OS) { OS << "; stage(" << BTA.getStage(V) << ')'; });
  }
};

// Color AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Displays the results by color-coding static parts of the IR, in addition to
// plain annotations.
class BindingTimeAnalysisColorAssemblyAnnotationWriter
    : public BindingTimeAnalysisPlainAssemblyAnnotationWriter {
  using Base = BindingTimeAnalysisPlainAssemblyAnnotationWriter;

public:
  explicit BindingTimeAnalysisColorAssemblyAnnotationWriter(
      BindingTimeAnalysis &BTA)
      : Base(BTA) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    if (BTA.getStage(BB) < BTA.getLastStage()) {
      setColor(OS);
    }

    Base::emitBasicBlockStartAnnot(BB, OS);
    resetColor(OS);
  }

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    // Set color for the static instruction.
    if (BTA.getStage(I) < BTA.getLastStage()) {
      setColor(OS);
    }
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    Base::printInfoComment(V, OS);

    // Reset color after a static instruction.
    if (BTA.getStage(&V) < BTA.getLastStage()) {
      resetColor(OS);
    }

    // Set the color for the name of the next static basic block.
    if (auto *Term = dyn_cast<TerminatorInst>(&V)) {
      Function::const_iterator BB(Term->getParent());
      Function::const_iterator NextBB(std::next(BB));

      if (NextBB != BB->getParent()->end() &&
          BTA.getStage(&*NextBB) < BTA.getLastStage()) {
        setColor(OS);
      }
    }
  }

private:
  static void setColor(formatted_raw_ostream &OS) {
    OS.changeColor(raw_ostream::YELLOW, false, false);
  }

  static void resetColor(formatted_raw_ostream &OS) {
    OS.resetColor();
  }
};

} // namespace llvm

namespace {

// AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Selects the implementation based on the output stream.
class BindingTimeAnalysisAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  explicit BindingTimeAnalysisAssemblyAnnotationWriter(BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitFunctionAnnot(const Function *F,
                         formatted_raw_ostream &OS) override {
    if (OS.has_colors()) {
      Impl.reset(new BindingTimeAnalysisColorAssemblyAnnotationWriter(BTA));
    } else {
      Impl.reset(new BindingTimeAnalysisPlainAssemblyAnnotationWriter(BTA));
    }

    Impl->emitFunctionAnnot(F, OS);
  }

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    Impl->emitBasicBlockStartAnnot(BB, OS);
  }

  void emitBasicBlockEndAnnot(const BasicBlock *BB,
                              formatted_raw_ostream &OS) override {
    Impl->emitBasicBlockEndAnnot(BB, OS);
  }

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    Impl->emitInstructionAnnot(I, OS);
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    Impl->printInfoComment(V, OS);
  }

private:
  BindingTimeAnalysis &BTA;
  std::unique_ptr<AssemblyAnnotationWriter> Impl;
};

} // namespace

void BindingTimeAnalysis::print(raw_ostream &OS, const Function &F) {
  BindingTimeAnalysisAssemblyAnnotationWriter AAW(*this);
  F.print(OS, &AAW);
}

char BindingTimeAnalysis::ID;

INITIALIZE_PASS(BindingTimeAnalysis, "bta", "Binding-Time Analysis", true, true)

namespace {

class BindingTimeAnalysisPrinter : public FunctionPass {
public:
  static char ID;

  BindingTimeAnalysisPrinter() : FunctionPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BindingTimeAnalysis>();
    AU.setPreservesAll();
  }

  bool runOnFunction(Function &F) override {
    getAnalysis<BindingTimeAnalysis>().print(errs(), F);
    return false;
  }
};

char BindingTimeAnalysisPrinter::ID;

} // namespace

INITIALIZE_PASS_BEGIN(BindingTimeAnalysisPrinter, "print-bta",
                      "Binding-Time Analysis Printer", false, true)
INITIALIZE_PASS_DEPENDENCY(BindingTimeAnalysis)
INITIALIZE_PASS_END(BindingTimeAnalysisPrinter, "print-bta",
                    "Binding-Time Analysis Printer", false, true)
