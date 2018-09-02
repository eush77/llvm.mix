//===- Mix.cpp ------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the IR specializer pass.
//
//===----------------------------------------------------------------------===//

#include "StagedIRBuilder.h"
#include "Types.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/BindingTimeAnalysis.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Mix.h"

#include <cassert>
#include <iterator>
#include <memory>
#include <vector>

using namespace llvm;
using namespace mix;

#define DEBUG_TYPE "mix"

namespace {

class Mix : public ModulePass {
public:
  static char ID;

  Mix() : ModulePass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BindingTimeAnalysis>();
  }

  bool runOnModule(Module &M) override;

private:
  Value *visitMixIRIntrinsicInst(IntrinsicInst &I);
};

char Mix::ID;

} // namespace

INITIALIZE_PASS_BEGIN(Mix, "mix", "Multi-Stage Compilation", false, false)
INITIALIZE_PASS_DEPENDENCY(BindingTimeAnalysis)
INITIALIZE_PASS_END(Mix, "mix", "Multi-Stage Compilation", false, false)

void llvm::addMixPass(PassManagerBuilder &PMB) {
  for (auto EP : {PassManagerBuilder::EP_EnabledOnOptLevel0,
                  PassManagerBuilder::EP_OptimizerLast}) {
    PMB.addExtension(EP, [](const auto &, auto &PM) { PM.add(new Mix); });
  }
}

bool Mix::runOnModule(Module &M) {
  DEBUG(dbgs() << "---- Mix : " << M.getName() << " ----\n");

  bool MadeChange = false;

  for (auto &F : M) {
    for (auto &BB : F) {
      for (BasicBlock::iterator BBI = BB.begin(), NextBBI; BBI != BB.end();
           BBI = NextBBI) {
        NextBBI = std::next(BBI);

        if (auto *I = dyn_cast<IntrinsicInst>(BBI)) {
          Value *V;

          switch (I->getIntrinsicID()) {
          case Intrinsic::mix_ir:
            V = visitMixIRIntrinsicInst(*I);
            break;

          default:
            continue;
          }

          V->takeName(I);
          I->replaceAllUsesWith(V);
          I->eraseFromParent();
          MadeChange = true;

          // Check if the basic block is split.
          if (NextBBI->getParent() != &BB) {
            NextBBI = BB.end();
          }
        }
      }
    }
  }

  return MadeChange;
}

namespace {

// Traverse dynamic blocks in the CFG starting from a given block and
// following dynamic control flow edges dynamic terminators.
//
// Outputs the starting block, then all dynamic blocks traversed, and then
// optionally the block with a static terminator (there must be at most one).
// Returns the static terminator if found.
template <typename OutputIt>
TerminatorInst *traverseDynamicBlocks(BasicBlock *Start, OutputIt Out,
                                      const BindingTimeAnalysis &BTA) {
  // The block with a static terminator reachable by dynamic edges from the
  // starting block.
  BasicBlock *Term = nullptr;

  for (auto B = df_begin(Start); B != df_end(Start);) {
    if (BTA.getStage((*B)->getTerminator()) < B->getParent()->getLastStage()) {
      assert(!Term &&
             "Multiple static terminators reachable from a static block");
      Term = *B;
      B.skipChildren();
      continue;
    }

    *Out++ = *B++;
  }

  if (Term) {
    *Out++ = Term;
    return Term->getTerminator();
  }

  return nullptr;
}

// Helper class for generating builder code for mixing one source function
// with static arguments.
class MixFunction {
public:
  MixFunction(StagedIRBuilder<IRBuilder<>> &SB, Function *F,
              Instruction *StagedModule, iterator_range<User::op_iterator> Args,
              const BindingTimeAnalysis &BTA, BasicBlock *InsertBefore)
      : SB(SB), Parent(StagedModule->getFunction()), F(F),
        StagedModule(StagedModule), Args(Args), BTA(BTA),
        InsertBefore(InsertBefore) {
    buildFunction();
  }

  Instruction *getFunction() { return StagedF; }

  // Generated code has one entry block and one exit block.
  BasicBlock *getEntry() const { return Entry; }
  BasicBlock *getExit() const { return Exit; }

  Value *getStaticReturnValue() { return StaticReturn; }

private:
  void buildFunction();
  void buildBasicBlock(BasicBlock *BB);
  void buildInstruction(Instruction *I);

  // Entry and exit blocks of the generated builder.
  BasicBlock *Entry = nullptr;
  BasicBlock *Exit = nullptr;

  PHINode *StaticReturn = nullptr;

  StagedIRBuilder<IRBuilder<>> &SB;
  Function *Parent;
  Function *F;
  Instruction *StagedModule;
  Instruction *StagedF = nullptr;
  iterator_range<User::op_iterator> Args;
  const BindingTimeAnalysis &BTA;
  BasicBlock *InsertBefore;
};

} // namespace

void MixFunction::buildFunction() {
  Entry =
      BasicBlock::Create(Parent->getContext(), Twine(F->getName()) + ".entry",
                         Parent, InsertBefore);
  Exit = BasicBlock::Create(Parent->getContext(), Twine(F->getName()) + ".exit",
                            Parent, InsertBefore);

  SB.getBuilder().SetInsertPoint(Entry);

  SmallVector<Type *, 4> DynamicArgTypes;
  SmallVector<StringRef, 4> DynamicArgNames;

  // Gather some info on dynamic arguments.
  for (Argument &A : F->args()) {
    if (A.getStage() == F->getLastStage()) {
      DynamicArgTypes.push_back(A.getType());
      DynamicArgNames.push_back(A.getName());
    }
  }

  StagedF = SB.createFunction(
      FunctionType::get(F->getReturnType(), DynamicArgTypes, false),
      GlobalValue::ExternalLinkage, F->getName(), StagedModule, F->getName());

  SB.createBuilder(StagedF, Twine(F->getName()) + ".builder");

  // Stage function arguments.
  {
    auto NextStaticArg = Args.begin();
    unsigned NextDynamicArg = 0;

    for (Argument &A : F->args()) {
      if (A.getStage() < F->getLastStage()) {
        assert(NextStaticArg != Args.end());
        SB.defineStatic(&A, *NextStaticArg++);
        SB.stageStatic(&A);
      } else {
        SB.setName(SB.stage(&A, NextDynamicArg),
                   DynamicArgNames[NextDynamicArg]);
        NextDynamicArg += 1;
      }
    }
  }

  SB.getBuilder().CreateBr(SB.defineStatic(&F->getEntryBlock()));

  // Create static return phi.
  if (F->getReturnStage() < F->getLastStage()) {
    SB.getBuilder().SetInsertPoint(Exit);
    StaticReturn = SB.getBuilder().CreatePHI(F->getReturnType(), 2,
                                             Twine(F->getName(), ".return"));
  }

  // Build static basic blocks in depth-first order.
  for (BasicBlock *BB : depth_first(F)) {
    if (BTA.getStage(BB) < F->getLastStage()) {
      buildBasicBlock(BB);
    }
  }

  if (StaticReturn) {
    DEBUG(dbgs() << "\nStatic return phi:\n" << *StaticReturn << '\n');
  }
}

void MixFunction::buildBasicBlock(BasicBlock *BB) {
  DEBUG(dbgs() << "Building static basic block ";
        BB->printAsOperand(dbgs(), false); dbgs() << ":\n");

  BasicBlock *Parent = SB.defineStatic(BB);
  Parent->moveBefore(Exit);
  SB.getBuilder().SetInsertPoint(Parent);

  // Build dynamic terminator in the dynamic predecessor block.
  if (BB != &F->getEntryBlock()) {
    SB.stage(
        std::unique_ptr<Instruction, ValueDeleter>(BranchInst::Create(BB)));
  }

  std::vector<BasicBlock *> Blocks;
  TerminatorInst *StaticTerm =
      traverseDynamicBlocks(BB, std::back_inserter(Blocks), BTA);

  for (auto *BB : Blocks) {
    DEBUG(dbgs() << "  "; BB->printAsOperand(dbgs(), false); dbgs() << '\n');

    SB.positionBuilderAtEnd(SB.stage(BB));

    for (auto &I : *BB) {
      buildInstruction(&I);
    }
  }

  if (StaticTerm) {
    DEBUG(dbgs() << *StaticTerm << "\n\n");

    if (!isa<ReturnInst>(StaticTerm))
      return;

    StaticReturn->addIncoming(
        SB.defineStatic(cast<ReturnInst>(StaticTerm)->getReturnValue()),
        Parent);
  } else {
    DEBUG(dbgs() << "  (no static terminator)\n\n");
  }

  SB.disposeBuilder();
  SB.getBuilder().CreateBr(Exit);
}

void MixFunction::buildInstruction(Instruction *I) {
  if (BTA.getStage(I) == F->getLastStage() || isa<ReturnInst>(I)) {
    SB.stage(I);
    return;
  }

  if (auto *Phi = dyn_cast<PHINode>(I)) {
    SB.defineStatic(Phi, BTA.getPhiValueBindingTime(Phi) == F->getLastStage());
  }

  SB.stageStatic(I);
}

Value *Mix::visitMixIRIntrinsicInst(IntrinsicInst &I) {
  Function *MixedF =
      cast<Function>(cast<ConstantExpr>(I.getArgOperand(0))->getOperand(0));

  const BindingTimeAnalysis &BTA = getAnalysis<BindingTimeAnalysis>(*MixedF);

  // Print Mix header after the analysis.
  DEBUG(dbgs() << "---- Mix : @" << MixedF->getName() << " ----\n"
               << "Creating code generator in @" << I.getFunction()->getName()
               << '\n'
               << I << "\n\n");

  IRBuilder<> B(&I);
  Value *StagedContext =
      B.CreateBitCast(I.getArgOperand(1), getContextPtrTy(I.getContext()),
                      I.getArgOperand(1)->getName());

  StagedIRBuilder<IRBuilder<>> SB(B, StagedContext);
  Instruction *StagedModule =
      SB.createModule(Twine(MixedF->getName(), ".module"), "module");

  // Split the call basic block.
  BasicBlock *Head = I.getParent();
  BasicBlock *Tail = Head->splitBasicBlock(&I, Head->getName());

  MixFunction Mix(SB, MixedF, StagedModule,
                  make_range(std::next(I.arg_begin(), 2), I.arg_end()), BTA,
                  Tail);
  cast<BranchInst>(Head->getTerminator())->setSuccessor(0, Mix.getEntry());
  BranchInst::Create(Tail, Mix.getExit());

  return new BitCastInst(Mix.getFunction(), I.getType(), "", &I);
}
