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

#include "llvm/ADT/SetVector.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "bta"

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");

  SetVector<const Instruction *> DynamicInstructions;

  // Push dynamic roots to DynamicInstructions and mark everything else
  // static.
  for (auto &I : instructions(F)) {
    if (I.getType()->isVoidTy() || I.mayHaveSideEffects() ||
        I.mayReadFromMemory() || isa<CallInst>(&I) || isa<AllocaInst>(&I)) {
      DEBUG({
        dbgs() << "Instruction";
        if (I.hasName()) {
          dbgs() << " %" << I.getName();
        }
        dbgs() << " is dynamic:\n" << I << '\n';
      });

      DynamicInstructions.insert(&I);
      BindingTimes[&I] = Dynamic;
    } else {
      BindingTimes[&I] = Static;
    }
  }

  // Mark every user of a dynamic value dynamic.
  for (unsigned DINum = 0; DINum < DynamicInstructions.size(); ++DINum) {
    const Instruction *DynInst = DynamicInstructions[DINum];

    for (auto &Use : DynInst->uses()) {
      if (auto *UserInst = dyn_cast<Instruction>(Use.getUser())) {
        if (!isStatic(UserInst))
          continue;

        DEBUG({
          dbgs() << "Instruction";
          if (UserInst->hasName()) {
            dbgs() << " %" << UserInst->getName();
          }
          dbgs() << " is dynamic (user of %" << DynInst->getName() << "):\n"
                 << *UserInst << '\n';
        });

        DynamicInstructions.insert(UserInst);
        BindingTimes[UserInst] = Dynamic;
      }
    }
  }

  return false;
}

bool BindingTimeAnalysis::isStatic(const Instruction *I) const {
  auto Iter = BindingTimes.find(I);
  assert(Iter != BindingTimes.end() && "Instruction has not been analyzed");

  return Iter->second == Static;
}

namespace {

class BindingTimeAnalysisAssemblyAnnotationWriter
    : public AssemblyAnnotationWriter {
public:
  BindingTimeAnalysisAssemblyAnnotationWriter(const BindingTimeAnalysis &BTA)
      : BTA(BTA) {}

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    if (BTA.isStatic(I) && OS.has_colors()) {
      OS.changeColor(raw_ostream::YELLOW, false, false);
    }
  }

  void printInfoComment(const Value &V, formatted_raw_ostream &OS) override {
    auto *I = dyn_cast<Instruction>(&V);

    if (!I || !BTA.isStatic(I))
      return;

    if (OS.has_colors()) {
      OS.resetColor();
    } else {
      OS.PadToColumn(40) << "; static";
    }
  }

private:
  const BindingTimeAnalysis &BTA;
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
