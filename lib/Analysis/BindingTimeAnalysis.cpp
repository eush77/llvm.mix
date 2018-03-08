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
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "bta"

bool BindingTimeAnalysis::runOnFunction(Function &F) {
  DEBUG(dbgs() << "---- BTA : " << F.getName() << " ----\n\n");
  return false;
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
