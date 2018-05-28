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

#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Printable.h"

#include <cassert>
#include <iterator>
#include <memory>
#include <queue>

using namespace llvm;

#define DEBUG_TYPE "bta"

namespace {

bool isPossiblyStaticTerminator(const Instruction *I) {
  if (!I->isTerminator())
    return false;

  switch (I->getOpcode()) {
  case Instruction::Br:
  case Instruction::Switch:
  case Instruction::IndirectBr:
    return true;

  default:
    return false;
  }
}

#ifndef NDEBUG
raw_ostream &dumpDynBB(const BasicBlock *BB) {
  return dbgs() << "Basic block %" << BB->getName() << " is dynamic";
}

raw_ostream &dumpDynInst(const Instruction *I) {
  dbgs() << "Instruction";

  if (I->hasName()) {
    dbgs() << " %" << I->getName();
  }

  return dbgs() << " is dynamic";
}
#endif

} // namespace

// Push dynamic roots to MarkedInstructions queue and make everything else
// static.
void BindingTimeAnalysis::initializeBindingTimeDivision(const Function &F) {
  MarkedInstructions.clear();
  NextMarkedInstructionNumber = 0;

  for (const auto &I : instructions(F)) {
    if ((I.getType()->isVoidTy() && !isPossiblyStaticTerminator(&I)) ||
        I.mayHaveSideEffects() || I.mayReadFromMemory() || isa<CallInst>(&I) ||
        isa<AllocaInst>(&I)) {
      DEBUG(dumpDynInst(&I) << ":\n" << I << '\n');
      InstructionBindingTimes[&I] = Dynamic;
      MarkedInstructions.insert(&I);
    } else {
      InstructionBindingTimes[&I] = Static;
    }
  }

  for (auto &BB : F) {
    BasicBlockBindingTimes[&BB] = Static;
  }
}

// Compute fixed-point basic block and instruction binding-time division by
// processing MarkedInstructions queue.
void BindingTimeAnalysis::computeBindingTimeDivision(const Function &F) {
  // Compute the fixed point of binding-time rules.
  while (NextMarkedInstructionNumber < MarkedInstructions.size()) {
    const Instruction *MarkedInst =
        MarkedInstructions[NextMarkedInstructionNumber++];

    // Mark instruction users.
    for (auto &Use : MarkedInst->uses()) {
      if (auto *UserInst = dyn_cast<Instruction>(Use.getUser())) {
        MarkedInstructions.insert(UserInst);

        if (isa<PHINode>(UserInst)) {
          // Don't change binding times of phis.
          continue;
        }

        DEBUG(dumpDynInst(UserInst)
              << " (user of %" << MarkedInst->getName() << "):\n"
              << *UserInst << '\n');
        InstructionBindingTimes[UserInst] = Dynamic;
      }
    }

    // Make successor blocks dynamic.
    if (auto *DynTerm = dyn_cast<TerminatorInst>(MarkedInst)) {
      for (auto *SuccBB : DynTerm->successors()) {
        DEBUG(dumpDynBB(SuccBB) << " (destination of terminator "
                                << DynTerm->getOpcodeName() << "):\n"
                                << *DynTerm << '\n');
        BasicBlockBindingTimes[SuccBB] = Dynamic;

        // Make phis dynamic.
        for (auto &Phi : SuccBB->phis()) {
          DEBUG(dumpDynInst(&Phi)
                << " (in dynamic basic block %" << SuccBB->getName() << "):\n"
                << Phi << '\n');
          InstructionBindingTimes[&Phi] = Dynamic;
          MarkedInstructions.insert(&Phi);
        }

        // Make terminators in predecessor blocks dynamic.
        for (auto *PredBB : predecessors(SuccBB)) {
          const TerminatorInst *PredTerm = PredBB->getTerminator();

          DEBUG(dumpDynInst(PredTerm) << " (branches to dynamic basic block %"
                                      << SuccBB->getName() << "):\n"
                                      << *PredTerm << '\n');
          InstructionBindingTimes[PredTerm] = Dynamic;
          MarkedInstructions.insert(PredTerm);
        }
      }
    }
  }
}

void BindingTimeAnalysis::computeStaticTerminators(const Function &F) {
  std::queue<const BasicBlock *> Worklist;

  // Initialize static terminators.
  for (const auto &BB : F) {
    const auto *Term = BB.getTerminator();

    if (getBindingTime(Term) == Static) {
      DEBUG(dbgs() << "Adding static terminator for %" << BB.getName() << ":\n"
                   << *Term << '\n');

      StaticTerminators[&BB] = Term;
      Worklist.push(&BB);
    } else {
      StaticTerminators.erase(&BB);
    }
  }

  while (!Worklist.empty()) {
    const auto *BB = Worklist.front();

    Worklist.pop();

    if (getBindingTime(BB) == Static)
      continue;

    const auto *Term = StaticTerminators.lookup(BB);

    // Check if the terminator has got marked.
    if (getBindingTime(Term) != Static)
      continue;

    for (const auto *PredBB : predecessors(BB)) {
      auto EmplaceResult = StaticTerminators.try_emplace(PredBB, Term);

      // Check if the map already contains a terminator.
      if (!EmplaceResult.second) {
        const auto *&PrevTerm = EmplaceResult.first->second;

        // Check if previous static terminator has got marked in the interim and
        // is no longer static, in which case replace it.
        if (getBindingTime(PrevTerm) != Static) {
          DEBUG(dbgs() << "Removing static terminator from %"
                       << PredBB->getName() << ":\n"
                       << *PrevTerm << '\n');

          PrevTerm = Term;
        }

        // If previous terminator is still static, then report a conflict and
        // make the current terminator dynamic.
        else {
          errs() << "Multiple static terminators for %" << PredBB->getName()
                 << ":\n"
                 << "  (         )" << *PrevTerm << '\n'
                 << "  (->dynamic)" << *Term << '\n';

          InstructionBindingTimes[Term] = Dynamic;
          MarkedInstructions.insert(Term);
          break;
        }
      }

      DEBUG(dbgs() << "Adding static terminator for %" << PredBB->getName()
                   << ":\n"
                   << *Term << '\n');

      Worklist.push(PredBB);
    }
  }
}

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");

  initializeBindingTimeDivision(F);

  do {
    computeBindingTimeDivision(F);
    computeStaticTerminators(F);
  } while (NextMarkedInstructionNumber < MarkedInstructions.size());

  return false;
}

BindingTimeAnalysis::BindingTime
BindingTimeAnalysis::getBindingTime(const BasicBlock *BB) const {
  auto Iter = BasicBlockBindingTimes.find(BB);
  assert(Iter != BasicBlockBindingTimes.end() &&
         "Basic block has not been analyzed");

  return Iter->second;
}

BindingTimeAnalysis::BindingTime
BindingTimeAnalysis::getBindingTime(const Instruction *I) const {
  auto Iter = InstructionBindingTimes.find(I);
  assert(Iter != InstructionBindingTimes.end() &&
         "Instruction has not been analyzed");

  return Iter->second;
}

BindingTimeAnalysis::StaticBasicBlockIterator &
BindingTimeAnalysis::StaticBasicBlockIterator::
operator=(const StaticBasicBlockIterator &Other) {
  assert(BTA == Other.BTA &&
         "Assigning static basic block iterator for a different analysis.");

  Term = Other.Term;
  MapIter = Other.MapIter;
  MapEnd = Other.MapEnd;
  return *this;
}

bool BindingTimeAnalysis::StaticBasicBlockIterator::
operator==(const StaticBasicBlockIterator &Other) const {
  assert(BTA == Other.BTA &&
         "Comparing static basic block iterators for different analyses.");

  return MapIter == Other.MapIter && MapEnd == Other.MapEnd &&
         Term == Other.Term;
}

BindingTimeAnalysis::StaticBasicBlockIterator &
    BindingTimeAnalysis::StaticBasicBlockIterator::operator++() {
  ++MapIter;
  skipToNextMapIter();
  return *this;
}

// Skip to next matching underlying iterator.
void BindingTimeAnalysis::StaticBasicBlockIterator::skipToNextMapIter() {
  while (MapIter != MapEnd &&
         (MapIter->second != Term ||
          BTA->getBindingTime(MapIter->first) != BindingTimeAnalysis::Static)) {
    ++MapIter;
  }
}

namespace {

Printable printSBB(const BindingTimeAnalysis &BTA, const TerminatorInst *Term) {
  assert(BTA.getBindingTime(Term) == BindingTimeAnalysis::Static);

  return Printable([=, &BTA](raw_ostream &OS) {
    OS << "sbb =";

    bool NeedComma = false;

    for (const auto &SBB : BTA.staticBasicBlocks(Term)) {
      OS << (NeedComma ? ", %" : " %") << SBB.getName();

      NeedComma = true;
    }
  });
}

// Plain AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Prints the results in plain-text comments beneath basic blocks and to the
// right of instructions.
class BindingTimeAnalysisPlainAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  BindingTimeAnalysisPlainAssemblyAnnotationWriter(
      const BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    if (BTA.getBindingTime(BB) == BindingTimeAnalysis::Static &&
        // Don't emit binding time for the entry block even though it's
        // static, since BindingTimeAnalysisColorAssemblyAnnotationWriter
        // is unable to do it.
        BB != &BB->getParent()->getEntryBlock() &&
        // If a block has no name and is unused, AssemblyWriter won't print
        // its label.
        (BB->hasName() || !BB->use_empty())) {
      OS << "; static\n";
    }
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    if (auto *I = dyn_cast<Instruction>(&V)) {
      if (BTA.getBindingTime(I) != BindingTimeAnalysis::Static)
        return;

      OS.PadToColumn(InfoCommentColumn) << "; static";

      // Print SBB set of this terminator.
      if (auto *Term = dyn_cast<TerminatorInst>(I)) {
        OS << ", " << printSBB(BTA, Term);
      }
    }
  }

private:
  static constexpr unsigned InfoCommentColumn = 42;

  const BindingTimeAnalysis &BTA;
};

// Color AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Displays the results by color-coding static parts of the IR.
class BindingTimeAnalysisColorAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  BindingTimeAnalysisColorAssemblyAnnotationWriter(
      const BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                formatted_raw_ostream &OS) override {
    // Reset color after a name of static basic block.
    resetColor(OS);
  }

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    // Set color for the static instruction.
    if (BTA.getBindingTime(I) == BindingTimeAnalysis::Static) {
      setColor(OS);
    }
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    auto *I = dyn_cast<Instruction>(&V);

    if (!I)
      return;

    // Print SBB set of static terminator.
    if (auto *Term = dyn_cast<TerminatorInst>(I)) {
      if (BTA.getBindingTime(Term) == BindingTimeAnalysis::Static) {
        OS.PadToColumn(InfoCommentColumn) << "; " << printSBB(BTA, Term);
      }
    }

    // Reset color after a static instruction.
    if (BTA.getBindingTime(I) == BindingTimeAnalysis::Static) {
      resetColor(OS);
    }

    // Set the color for the name of the next static basic block.
    if (auto *Term = dyn_cast<TerminatorInst>(I)) {
      Function::const_iterator BB(Term->getParent());
      Function::const_iterator NextBB(std::next(BB));

      if (NextBB != BB->getParent()->end() &&
          BTA.getBindingTime(&*NextBB) == BindingTimeAnalysis::Static) {
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

  static constexpr unsigned InfoCommentColumn = 50;

  const BindingTimeAnalysis &BTA;
};

// AssemblyAnnotationWriter for BindingTimeAnalysis.
//
// Selects the implementation based on the output stream.
class BindingTimeAnalysisAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  BindingTimeAnalysisAssemblyAnnotationWriter(const BindingTimeAnalysis &BTA)
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
  const BindingTimeAnalysis &BTA;
  std::unique_ptr<AssemblyAnnotationWriter> Impl;
};

} // namespace

void BindingTimeAnalysis::print(raw_ostream &OS, const Function &F) const {
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
