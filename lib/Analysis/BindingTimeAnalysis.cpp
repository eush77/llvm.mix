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
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstIterator.h"
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
#include <string>
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
  case Instruction::Ret:
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
  unsigned NumDirectUsers = TransitiveUsers.size();

  for (unsigned TUNum = 0; TUNum < TransitiveUsers.size(); ++TUNum) {
    const Value *TU = TransitiveUsers[TUNum];

    // Close the set of transitive users by including users of phi users.
    if (auto *Phi = dyn_cast<PHINode>(TU)) {
      for (const Value *V : Phi->users()) {
        TransitiveUsers.insert(V);
      }
    } else {
      enqueue(TU, IncomingStage,
              TUNum < NumDirectUsers ? WorklistItem::Operand
                                     : WorklistItem::TransitiveOperand,
              V);
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
    enqueue(PredBB->getTerminator(), Stage, WorklistItem::Successor, BB);
  }

  for (const PHINode &Phi : BB->phis()) {
    enqueue(&Phi, Stage, WorklistItem::Parent, BB);
  }
}

void BindingTimeAnalysis::fixInstruction(const Instruction *I,
                                         unsigned MaxOperandStage) {
  if (!updateStage(I, MaxOperandStage))
    return;

  addTransitiveNonPhiUsers(I, MaxOperandStage);

  if (auto *Term = dyn_cast<TerminatorInst>(I)) {
    for (const BasicBlock *BB : Term->successors()) {
      enqueue(BB, MaxOperandStage, WorklistItem::PredTerminator, Term);
    }

    if (isa<ReturnInst>(I) && F->getReturnStage() < MaxOperandStage) {
      F->getContext().diagnose(DiagnosticInfoBindingTime(
          DS_Error,
          "Inferred stage(" + Twine(MaxOperandStage) +
              (") contradicts the declared return stage of @") + F->getName(),
          I));
    }
  }
}

void BindingTimeAnalysis::initializeWorklist() {
  assert(Worklist.empty() && "Unfinished analysis");

  // Add function arguments to the worklist.
  for (const Argument &A : F->args()) {
    enqueue(&A, A.getStage(), WorklistItem::Attribute);
  }

  // Add basic blocks and instructions to the worklist.
  for (const BasicBlock &BB : *F) {
    enqueue(&BB, 0, WorklistItem::Default);

    for (const Instruction &I : BB) {
      if (isLastStage(&I)) {
        enqueue(&I, F->getLastStage(), WorklistItem::LastStage);
      } else if (isa<ReturnInst>(I)) {
        enqueue(&I, F->getReturnStage(), WorklistItem::ReturnStage);
      } else {
        enqueue(&I, 0, WorklistItem::Default);
      }
    }
  }
}

// At each stage, every basic block has to have at most one terminator.
void BindingTimeAnalysis::fixTerminators() {
  for (auto &SourceBB : *F) {
    for (unsigned Stage = getStage(&SourceBB); Stage <= F->getLastStage();
         ++Stage) {
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

        // Favor function returns.
        if (isa<ReturnInst>(T) && !isa<ReturnInst>(Term)) {
          std::swap(Term, T);
        }

        enqueue(T, Stage + 1, WorklistItem::StageTerminator, Term, &SourceBB);

        // Don't emit the diagnostic in the debug mode.
        DEBUG(continue);

        F->getContext().diagnose(DiagnosticInfoBindingTime(
            DS_Warning,
            "Multiple stage(" + Twine(Stage) + ") terminators of %" +
                SourceBB.getName(),
            Term));
        F->getContext().diagnose(DiagnosticInfoBindingTime(
            DS_Note,
            "The other terminator is moved to stage(" + Twine(Stage + 1) + ")",
            T));
      }
    }
  }
}

// Compute a fixed point of binding-time rules.
void BindingTimeAnalysis::fix() {
  do {
    while (auto WI = popItem()) {
      unsigned IS = WI->getIncomingStage();

      DEBUG(WI->print(dbgs(), *this));

      if (auto *A = WI->get<Argument>()) {
        fixArgument(A, IS);
      } else if (auto *BB = WI->get<BasicBlock>()) {
        fixBasicBlock(BB, IS);
      } else if (auto *I = WI->get<Instruction>()) {
        fixInstruction(I, IS);
      } else {
        llvm_unreachable("Unsupported value kind");
      }
    }

    fixTerminators();

    DEBUG(dbgs() << '\n');
  } while (!Worklist.empty());
}

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  if (!F.isStaged())
    return false;

  DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");

  this->F = &F;

  // Initialize slot tracker for printing.
  MST.emplace(F.getParent());
  MST->incorporateFunction(F);

  // Reset previous state for this function.
  WorklistID = 0;
  std::for_each(F.begin(), F.end(), [&](auto &V) { StageMap.erase(&V); });
  std::for_each(inst_begin(F), inst_end(F),
                [&](auto &V) { StageMap.erase(&V); });

  // Default to stage(0) in `getStage'.
  SaveAndRestore<bool> SavedDTS0(DefaultToStage0, true);

  initializeWorklist();
  fix();
  return false;
}

// True if the value is analyzed and has a binding time.
bool BindingTimeAnalysis::isAnalyzed(const Value *V) const {
  return isa<Constant>(V) || StageMap.count(V);
}

unsigned BindingTimeAnalysis::getStage(const Value *V) const {
  assert((DefaultToStage0 || isAnalyzed(V)) && "Value has not been analyzed");

  if (isa<Constant>(V))
    return 0;

  return StageMap.lookup(V);
}

unsigned BindingTimeAnalysis::getPhiValueBindingTime(const PHINode *Phi) const {
  return std::accumulate(Phi->op_begin(), Phi->op_end(), 0,
                         [this](unsigned Stage, const Value *V) {
                           return std::max(Stage, getStage(V));
                         });
}

// Add new item to the worklist, but not if the value has already surpassed
// the incoming stage.
template <typename... ArgsT>
void BindingTimeAnalysis::enqueue(const Value *V, unsigned IncomingStage,
                                  WorklistItem::Reason R, ArgsT... Args) {
  if (!isAnalyzed(V) || getStage(V) < IncomingStage) {
    Worklist.emplace(WorklistID++, V, IncomingStage, R, Args...);
  }
}

// Pop off next unsuperseded item from the worklist.
Optional<BindingTimeAnalysis::WorklistItem> BindingTimeAnalysis::popItem() {
  // Drop old items.
  while (!Worklist.empty() && isAnalyzed(Worklist.top().get<Value>()) &&
         Worklist.top().getIncomingStage() <=
             getStage(Worklist.top().get<Value>())) {
    Worklist.pop();
  }

  if (Worklist.empty())
    return None;

  auto WI = Worklist.top();
  Worklist.pop();
  return WI;
}

void BindingTimeAnalysis::WorklistItem::print(raw_ostream &OS,
                                              BindingTimeAnalysis &BTA) const {
  OS << BTA.printValueWithKind(V) << " to stage(" << InStage << ')'
     << (Sender ? " from " : " by ");

  switch (R) {
  case Attribute:
    OS << "attribute";
    break;

  case Default:
    OS << "default";
    break;

  case LastStage:
    OS << "last stage";
    break;

  case Operand:
    OS << "operand";
    break;

  case Parent:
    OS << "parent";
    break;

  case PredTerminator:
    OS << "predecessor terminator";
    break;

  case ReturnStage:
    OS << "return stage";
    break;

  case StageTerminator:
    OS << "stage(" << (InStage - 1) << ") terminator of "
       << BTA.printName(cast<BasicBlock>(Arg));
    break;

  case Successor:
    OS << "successor";
    break;

  case TransitiveOperand:
    OS << "transitive operand";
    break;
  }

  if (Sender) {
    OS << ' ' << BTA.printName(Sender);
  }

  OS << '\n';

  if (auto *I = dyn_cast<Instruction>(V)) {
    OS << BTA.printInstWithBlock(I);
  }
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

constexpr unsigned CommentColumn = 50;

// Print instruction and comment it with the name of the parent basic block.
Printable BindingTimeAnalysis::printInstWithBlock(const Instruction *I,
                                                  StringRef Prefix) {
  return Printable([=](raw_ostream &OS) {
    formatted_raw_ostream FOS(OS);

    FOS << Prefix;
    I->print(FOS, *MST);
    FOS.PadToColumn(CommentColumn)
        << "; in " << printName(I->getParent()) << '\n';
  });
}

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
  BindingTimeAnalysisColorAssemblyAnnotationWriter(const Function &F,
                                                   BindingTimeAnalysis &BTA)
      : Base(BTA), F(F) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    if (BTA.getStage(BB) < F.getLastStage()) {
      setColor(OS);
    }

    Base::emitBasicBlockStartAnnot(BB, OS);
    resetColor(OS);
  }

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    // Set color for the static instruction.
    if (BTA.getStage(I) < F.getLastStage()) {
      setColor(OS);
    }
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    Base::printInfoComment(V, OS);

    // Reset color after a static instruction.
    if (BTA.getStage(&V) < F.getLastStage()) {
      resetColor(OS);
    }

    // Set the color for the name of the next static basic block.
    if (auto *Term = dyn_cast<TerminatorInst>(&V)) {
      Function::const_iterator BB(Term->getParent());
      Function::const_iterator NextBB(std::next(BB));

      if (NextBB != BB->getParent()->end() &&
          BTA.getStage(&*NextBB) < F.getLastStage()) {
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

  const Function &F;
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
      Impl.reset(new BindingTimeAnalysisColorAssemblyAnnotationWriter(*F, BTA));
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
  assert(F.isStaged() && "Function is not staged");

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
    if (F.isStaged()) {
      getAnalysis<BindingTimeAnalysis>().print(errs(), F);
    }
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

void DiagnosticInfoBindingTime::print(DiagnosticPrinter &DP) const {
  DP << Msg;

  if (!I)
    return;

  std::string S;
  raw_string_ostream OS(S);
  formatted_raw_ostream FOS(OS);

  FOS << ":\n" << *I;
  FOS.PadToColumn(CommentColumn) << "; in %" << I->getParent()->getName();
  FOS.flush();

  DP << S;
}
