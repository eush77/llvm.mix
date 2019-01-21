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
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Printable.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <iterator>
#include <memory>
#include <numeric>
#include <string>
#include <utility>

using namespace llvm;
using namespace std::placeholders;

#define DEBUG_TYPE "bta"

static cl::opt<bool> PrintBTA("print-bta", cl::Hidden,
                              cl::desc("Print binding-time analysis results"));

const IntrinsicInst *llvm::getObjectStageAnnotation(const Value *V) {
  // Look through inbounds GEP
  while (auto *GEP = dyn_cast<GetElementPtrInst>(V)) {
    if (GEP->isInBounds())
      V = GEP->getPointerOperand();
    else
      break;
  }

  if (auto *I = dyn_cast<IntrinsicInst>(V)) {
    if (I->getIntrinsicID() == Intrinsic::object_stage) {
      return I;
    }
  }
  return nullptr;
}

// Get the earliest stage at which the pointed object is constant. If it is
// not constant at any stage, return `LastStage + 1'.
unsigned BindingTimeAnalysis::getObjectStage(const Value *V) const {
  if (auto *I = getObjectStageAnnotation(V)) {
    return std::min<unsigned>(
        cast<ConstantInt>(I->getOperand(1))->getZExtValue(),
        F->getLastStage() + 1);
  }
  return F->getLastStage() + 1;
}

// True if the instruction must be evaluated at the last stage.
bool BindingTimeAnalysis::isLastStage(const Instruction *I) const {
  switch (I->getOpcode()) {
  case Instruction::Br:
  case Instruction::Call:
  case Instruction::IndirectBr:
  case Instruction::Load:
  case Instruction::Ret:
  case Instruction::Store:
  case Instruction::Switch:
    return false;

  case Instruction::Alloca:
  case Instruction::Invoke:
    return true;

  default:
    return I->getType()->isVoidTy() || I->mayHaveSideEffects() ||
           I->mayReadFromMemory();
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

// Verify incoming stage for instruction.
void BindingTimeAnalysis::verifyUse(const Use &U, unsigned IncomingStage) {
  LLVMContext &Ctx = U->getContext();

  if (auto *C = dyn_cast<CallInst>(U.getUser())) {
    Function *Callee = C->getCalledFunction();

    if (!Callee || !Callee->isStaged() ||
        Callee->arg_size() <= U.getOperandNo())
      return;

    Argument *A = &Callee->arg_begin()[U.getOperandNo()];

    if (A->getStage() < IncomingStage) {
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Note, *Callee, "Given the declared parameter %s of %s", A,
          A->getStage()));
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Note, *F, "Given the argument %s of %s", U, IncomingStage));
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Error, *F,
          "Inferred argument stage contradicts the declared parameter stage",
          C));
    }
  } else if (auto *S = dyn_cast<StoreInst>(U.getUser())) {
    unsigned ObjectStage = getObjectStage(S->getPointerOperand());
    StringRef OpName = U.getOperandNo() == StoreInst::getPointerOperandIndex()
                           ? "pointer"
                           : "value";

    if (ObjectStage <= IncomingStage) {
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Note, *F,
          "Given the declared object stage(" + Twine(ObjectStage) + ")",
          getObjectStageAnnotation(S->getPointerOperand())));
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Note, *F, "Given the " + OpName + " operand %s of %s", U,
          IncomingStage));
      U->getContext().diagnose(DiagnosticInfoBindingTime(
          DS_Error, *F,
          "Inferred " + OpName + " stage contradicts the object stage at store",
          S));
    }
  } else if (auto *R = dyn_cast<ReturnInst>(U.getUser())) {
    Function *F = R->getFunction();

    if (F->getReturnStage() < IncomingStage) {
      Ctx.diagnose(DiagnosticInfoBindingTime(DS_Note, *F,
                                             "Given the declared return %0.s%s",
                                             F, F->getReturnStage()));
      Ctx.diagnose(DiagnosticInfoBindingTime(
          DS_Error, *F,
          "Inferred return %.0s%s contradicts the declared return stage", R,
          IncomingStage));
    }
  }
}

// Add transitive non-phi users of a value to the worklist with a given
// incoming stage value.
void BindingTimeAnalysis::addTransitiveNonPhiUsers(const Value *V,
                                                   unsigned IncomingStage) {
  SmallSetVector<const Use *, 4> TransitiveUses;

  auto addUsesOf = [&](const Value *V) {
    for (const Use &U : V->uses()) {
      TransitiveUses.insert(&U);
    }
  };

  addUsesOf(V);
  unsigned NumDirectUsers = TransitiveUses.size();

  for (unsigned TUNum = 0; TUNum < TransitiveUses.size(); ++TUNum) {
    const Use &U = *TransitiveUses[TUNum];

    // Close the set of transitive users by including users of phi users.
    if (auto *Phi = dyn_cast<PHINode>(U.getUser())) {
      addUsesOf(Phi);
      continue;
    }

    verifyUse(U, IncomingStage);

    // Return stages of calls are declared with annotations
    if (isa<CallInst>(U.getUser()) && !getObjectStageAnnotation(U.getUser()))
      continue;

    enqueue(U.getUser(), IncomingStage,
            TUNum < NumDirectUsers ? WorklistItem::Operand
                                   : WorklistItem::TransitiveOperand,
            V);
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

  if (I->isTerminator()) {
    for (const BasicBlock *BB : successors(I)) {
      enqueue(BB, MaxOperandStage, WorklistItem::PredTerminator, I);
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
      } else if (auto *Call = dyn_cast<CallInst>(&I)) {
        Function *Callee = Call->getCalledFunction();

        if (getObjectStageAnnotation(Call)) {
          enqueue(&I, 0, WorklistItem::Default);
        } else if (Callee && Callee->isStaged() &&
                   !Callee->getReturnType()->isVoidTy()) {
          enqueue(&I, Callee->getReturnStage(), WorklistItem::CallStage);
        } else {
          enqueue(&I, F->getLastStage(), WorklistItem::LastStage);
        }
      } else if (auto *Load = dyn_cast<LoadInst>(&I)) {
        enqueue(&I,
                std::min(getObjectStage(Load->getPointerOperand()),
                         F->getLastStage()),
                WorklistItem::ObjectStage);
      } else if (isa<ReturnInst>(I)) {
        enqueue(&I, F->getReturnStage(), WorklistItem::ReturnStage);
      } else if (auto *Store = dyn_cast<StoreInst>(&I)) {
        const Value *P = Store->getPointerOperand();
        unsigned ObjectStage = getObjectStage(P);

        if (!ObjectStage) {
          I.getContext().diagnose(DiagnosticInfoBindingTime(
              DS_Note, *F, "Given the declared object stage",
              getObjectStageAnnotation(P)));
          I.getContext().diagnose(DiagnosticInfoBindingTime(
              DS_Error, *F, "Storing to a stage(0) object", Store));
        }

        enqueue(&I, ObjectStage - 1, WorklistItem::ObjectStage);
      } else {
        enqueue(&I, 0, WorklistItem::Default);
      }
    }
  }
}

// At each stage, every basic block has to have at most one unique terminator.
void BindingTimeAnalysis::fixTerminators() {
  for (auto &SourceBB : *F) {
    for (unsigned Stage = getStage(&SourceBB); Stage <= F->getLastStage();
         ++Stage) {
      const Instruction *Term = nullptr;

      for (auto BBIter = df_begin(&SourceBB); BBIter != df_end(&SourceBB);) {
        const Instruction *T = (*BBIter)->getTerminator();

        if (Stage < getStage(T)) {
          ++BBIter;
          continue;
        }

        BBIter.skipChildren();

        if (!Term) {
          Term = T;
          continue;
        }

        if (Term->isIdenticalTo(T))
          continue;

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
        LLVM_DEBUG(continue);

        F->getContext().diagnose(DiagnosticInfoBindingTime(
            DS_Warning, *F,
            "Multiple stage(" + Twine(Stage) + ") terminators of " +
                (SourceBB.hasName() ? "basic block %%" + SourceBB.getName()
                                    : "the entry block"),
            Term, getStage(Term)));
        F->getContext().diagnose(DiagnosticInfoBindingTime(
            DS_Note, *F,
            "Previous terminator is moved to stage(" + Twine(Stage + 1) + ")",
            T, getStage(T)));
      }
    }
  }
}

// Compute a fixed point of binding-time rules.
void BindingTimeAnalysis::fix() {
  do {
    while (auto WI = popItem()) {
      unsigned IS = WI->getIncomingStage();

      LLVM_DEBUG(WI->print(dbgs(), *this));

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

    LLVM_DEBUG(dbgs() << '\n');
  } while (!Worklist.empty());
}

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  LLVM_DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");

  this->F = &F;

  if (!F.isStaged()) {
    LLVM_DEBUG(dbgs() << "Function is not staged\n\n");
    return false;
  }

  // Initialize slot tracker for printing.
  MST.emplace(F.getParent());
  MST->incorporateFunction(F);

  // Reset previous state for this function.
  WorklistID = 0;
  std::for_each(F.arg_begin(), F.arg_end(),
                [&](auto &V) { StageMap.erase(&V); });
  std::for_each(F.begin(), F.end(), [&](auto &V) { StageMap.erase(&V); });
  std::for_each(inst_begin(F), inst_end(F),
                [&](auto &V) { StageMap.erase(&V); });

  // Default to stage(0) in `getStage'.
  SaveAndRestore<bool> SavedDTS0(DefaultToStage0, true);

  initializeWorklist();
  fix();

  if (PrintBTA)
    print(dbgs(), F.getParent());

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

#ifndef NDEBUG

namespace {

template <typename Container> struct ForEach;

template <typename T>
struct ForEach<std::priority_queue<T>> : std::priority_queue<T> {
  template <typename Func> ForEach(const std::priority_queue<T> &Q, Func &&F) {
    std::for_each(static_cast<const ForEach &>(Q).c.begin(),
                  static_cast<const ForEach &>(Q).c.end(),
                  std::forward<Func>(F));
  }
};

} // namespace

void BindingTimeAnalysis::dumpWorklist() const {
  dbgs() << "Worklist:\n";

  ForEach<std::priority_queue<WorklistItem>>(
      Worklist, std::bind(&BindingTimeAnalysis::WorklistItem::dump, _1));
}

#define FOR_EACH_WORKLIST_REASON_1(V, A)                                       \
  V(A, Attribute)                                                              \
  V(A, CallStage)                                                              \
  V(A, Default)                                                                \
  V(A, LastStage)                                                              \
  V(A, ObjectStage)                                                            \
  V(A, Operand)                                                                \
  V(A, Parent)                                                                 \
  V(A, PredTerminator)                                                         \
  V(A, ReturnStage)                                                            \
  V(A, StageTerminator)                                                        \
  V(A, Successor)                                                              \
  V(A, TransitiveOperand)

#define CASE_STRINGIFY(S, R)                                                   \
  case R:                                                                      \
    S = #R;                                                                    \
    break;

void BindingTimeAnalysis::WorklistItem::dump() const {
  auto dumpField = [&, Delim = "  "](const char *Field, auto &&V) mutable {
    dbgs() << Delim << Field << "=" << V;
    Delim = ", ";
  };

  auto dumpValueField = [&](const char *Field, const Value *V) {
    if (V) {
      dumpField(Field,
                Printable([V](raw_ostream &OS) { V->printAsOperand(OS); }));
    }
  };

  auto dumpReasonField = [&](const char *Field, Reason R) {
    const char *S = "Unknown";
    switch (R) { FOR_EACH_WORKLIST_REASON_1(CASE_STRINGIFY, S) }
    dumpField(Field, S);
  };

  dumpField("ID", ID);
  dumpValueField("V", V);
  dumpField("InStage", InStage);
  dumpReasonField("Reason", R);
  dumpValueField("Sender", Sender);
  dumpValueField("Arg", Arg);
  dbgs() << '\n';
}

#endif // NDEBUG

void BindingTimeAnalysis::WorklistItem::print(raw_ostream &OS,
                                              BindingTimeAnalysis &BTA) const {
  OS << BTA.printValueWithKind(V) << " to stage(" << InStage << ')'
     << (Sender ? " from " : " by ");

  switch (R) {
  case Attribute:
    OS << "attribute";
    break;

  case CallStage:
    OS << "call stage";
    break;

  case Default:
    OS << "default";
    break;

  case LastStage:
    OS << "last stage";
    break;

  case ObjectStage:
    OS << "object stage";
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
      const BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    OS << printStage(BB) << '\n';
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    OS.PadToColumn(CommentColumn) << printStage(&V);
  }

protected:
  const BindingTimeAnalysis &BTA;

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
  BindingTimeAnalysisColorAssemblyAnnotationWriter(
      const Function &F, const BindingTimeAnalysis &BTA)
      : Base(BTA), F(F) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override;

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override;

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override;

private:
  static void setColor(formatted_raw_ostream &OS) {
    OS.changeColor(raw_ostream::YELLOW, false, false);
  }

  static void resetColor(formatted_raw_ostream &OS) { OS.resetColor(); }

  const Function &F;
};

} // namespace llvm

void BindingTimeAnalysisColorAssemblyAnnotationWriter::emitBasicBlockStartAnnot(
    const BasicBlock *BB, formatted_raw_ostream &OS) {
  if (BTA.getStage(BB) < F.getLastStage())
    setColor(OS);

  Base::emitBasicBlockStartAnnot(BB, OS);
  resetColor(OS);
}

void BindingTimeAnalysisColorAssemblyAnnotationWriter::emitInstructionAnnot(
    const Instruction *I, formatted_raw_ostream &OS) {
  // Set color for the static instruction.
  if (BTA.getStage(I) < F.getLastStage())
    setColor(OS);
}

void BindingTimeAnalysisColorAssemblyAnnotationWriter::printInfoComment(
    const Value &V, formatted_raw_ostream &OS) {
  Base::printInfoComment(V, OS);

  // Reset color after a static instruction.
  if (BTA.getStage(&V) < F.getLastStage())
    resetColor(OS);

  // Set the color for the name of the next static basic block.
  if (auto *I = dyn_cast<Instruction>(&V)) {
    if (I->isTerminator()) {
      Function::const_iterator BB(I->getParent());
      Function::const_iterator NextBB(std::next(BB));

      if (NextBB != BB->getParent()->end() &&
          BTA.getStage(&*NextBB) < F.getLastStage())
        setColor(OS);
    }
  }
}

namespace {

// AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Selects the implementation based on the output stream.
class BindingTimeAnalysisAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  explicit BindingTimeAnalysisAssemblyAnnotationWriter(
      const BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitFunctionAnnot(const Function *F, formatted_raw_ostream &OS) override;

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
  const BindingTimeAnalysis &BTA;
  std::unique_ptr<AssemblyAnnotationWriter> Impl;
};

} // namespace

void BindingTimeAnalysisAssemblyAnnotationWriter::emitFunctionAnnot(
    const Function *F, formatted_raw_ostream &OS) {
  if (OS.has_colors()) {
    Impl.reset(new BindingTimeAnalysisColorAssemblyAnnotationWriter(*F, BTA));
  } else {
    Impl.reset(new BindingTimeAnalysisPlainAssemblyAnnotationWriter(BTA));
  }

  Impl->emitFunctionAnnot(F, OS);
}

void BindingTimeAnalysis::print(raw_ostream &OS, const Module *) const {
  assert(F && "No function to print");

  if (!F->isStaged())
    return;

  BindingTimeAnalysisAssemblyAnnotationWriter AAW(*this);
  F->print(OS, &AAW);
}

char BindingTimeAnalysis::ID;

INITIALIZE_PASS(BindingTimeAnalysis, "bta", "Binding-Time Analysis", true, true)

static DiagnosticLocation getLocation(const Value *V) {
  if (auto *A = dyn_cast<Argument>(V)) {
    return A->getParent()->getSubprogram();
  } else if (auto *F = dyn_cast<Function>(V)) {
    return F->getSubprogram();
  } else if (auto *I = dyn_cast<Instruction>(V)) {
    return I->getDebugLoc();
  }
  return {};
}

DiagnosticInfoBindingTime::DiagnosticInfoBindingTime(
    DiagnosticSeverity Severity, const Function &F, const Twine &Fmt,
    const Value *V, Optional<unsigned> Stage)
    : DiagnosticInfoWithLocationBase(DK_BindingTime, Severity, F,
                                     ::getLocation(V)),
      V(V), Stage(Stage) {
  std::string ValStr;
  raw_string_ostream ValOS(ValStr);
  V->printAsOperand(ValOS, true, F.getParent());
  ValOS.flush();

  SmallString<9> StageStr;
  if (Stage) {
    ("stage(" + Twine(*Stage) + ")").toVector(StageStr);
  }

  raw_string_ostream(Msg) << format(Fmt.str().c_str(), ValStr.c_str(),
                                    StageStr.c_str());
}

void DiagnosticInfoBindingTime::print(DiagnosticPrinter &DP) const {
  if (isLocationAvailable()) {
    DP << getLocationStr() << ": ";
  }

  DP << "in " << getFunction().getName() << ": " << Msg;

  auto *I = dyn_cast<Instruction>(V);

  if (!I)
    return;

  DP << ":\n";

  std::string S;
  raw_string_ostream OS(S);
  formatted_raw_ostream FOS(OS);

  FOS << *I;

  auto annotate = [&, StartComment = true]() mutable -> raw_ostream & {
    if (StartComment) {
      StartComment = false;
      return FOS.PadToColumn(CommentColumn) << "; ";
    } else {
      return FOS << ", ";
    }
  };

  if (Stage) {
    annotate() << "stage(" << *Stage << ')';
  }

  if (I->getParent()->hasName()) {
    annotate() << "in %" << I->getParent()->getName();
  }

  FOS.flush();
  DP << S;
}
