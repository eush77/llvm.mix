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

#include "MixContext.h"
#include "StagedIRBuilder.h"
#include "Types.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/BindingTimeAnalysis.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/InitializePasses.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
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
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/PassRegistry.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Mix.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <iterator>
#include <memory>
#include <utility>
#include <vector>

using namespace llvm;
using namespace mix;
using namespace std::placeholders;

#define DEBUG_TYPE "mix"

namespace {

class Mix : public ModulePass {
public:
  static char ID;

  Mix() : ModulePass(ID) {
    initializeMixPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BindingTimeAnalysis>();
    AU.addRequired<CallGraphWrapperPass>();
  }

  bool runOnModule(Module &) override;

private:
  struct StagedFunctionInfo {
    Function *Mix;
    FunctionType *FTy;
    GlobalValue::LinkageTypes Linkage;
  };

  Value *visitMixIRIntrinsicInst(IntrinsicInst &);
  StagedFunctionInfo declareFunction(Function &,
                                     GlobalValue::LinkageTypes) const;
  Function *buildMain(Function &F, Function &SourceF, MixContextTable &) const;
  void buildFunction(Function &SourceF, const StagedFunctionInfo &SFI,
                     MixContextTable &);
  void buildBasicBlock(BasicBlock &) const;
  void buildInstruction(Instruction &) const;
  bool buildCall(CallInst &) const;
  void buildRet(Value &) const;
  void buildRetVoid() const;

  Module *M = nullptr;
  const CallGraph *CG = nullptr;
  BindingTimeAnalysis *BTA = nullptr;
  IRBuilder<> *B = nullptr;
  DenseMap<Function *, StagedFunctionInfo> FunctionMap;
  MixContext *MC = nullptr;
  StagedIRBuilder<IRBuilder<>> *SB = nullptr;
  Function *SourceF = nullptr;
};

char Mix::ID;

} // namespace

INITIALIZE_PASS_BEGIN(Mix, "mix", "Multi-Stage Compilation", false, false)
INITIALIZE_PASS_DEPENDENCY(BindingTimeAnalysis)
INITIALIZE_PASS_DEPENDENCY(CallGraphWrapperPass)
INITIALIZE_PASS_END(Mix, "mix", "Multi-Stage Compilation", false, false)

void llvm::addMixPass(PassManagerBuilder &PMB) {
  for (auto EP : {PassManagerBuilder::EP_EnabledOnOptLevel0,
                    PassManagerBuilder::EP_OptimizerLast})
    PMB.addExtension(EP, [](const auto &, auto &PM) { PM.add(new Mix); });
}

bool Mix::runOnModule(Module &M) {
  SaveAndRestore<Module *> RestoreMOnExit(this->M, &M);
  SaveAndRestore<const CallGraph *> RestoreCGOnExit(
      CG, &getAnalysis<CallGraphWrapperPass>().getCallGraph());
  auto ClearBTAOnExit = make_scope_exit([this]() { BTA = nullptr; });
  IRBuilder<> B(M.getContext());
  SaveAndRestore<IRBuilder<> *> RestoreBOnExit(this->B, &B);

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

Value *Mix::visitMixIRIntrinsicInst(IntrinsicInst &I) {
  Function *MainF =
      cast<Function>(cast<ConstantExpr>(I.getArgOperand(0))->getOperand(0));

  // Run BTA on function. Skip first invocation if BTA is not initialized,
  // because its initialization involves running BTA on the entry function.
  auto RunBTA = [ this, SkipNext = !BTA ](Function & F) mutable {
    if (SkipNext)
      SkipNext = false;
    else
      BTA->runOnFunction(F);
  };

  // Initialize BTA.
  if (!BTA)
    BTA = &getAnalysis<BindingTimeAnalysis>(*MainF);

  // All functions staged for this intrinsic call
  std::vector<Function *> Functions;

  // Analyze all called functions
  const CallGraphNode *MixedCGN = (*CG)[MainF];
  for (auto CGN = df_begin(MixedCGN); CGN != df_end(MixedCGN);) {
    auto *F = CGN->getFunction();

    if (F && F->isStaged()) {
      RunBTA(*F);
      Functions.push_back(F);
      ++CGN;
    } else {
      CGN.skipChildren();
    }
  }

  // Print Mix header after the analysis.
  DEBUG(dbgs() << "---- Mix : @" << MainF->getName() << " ----\n"
               << "Creating code generator in @" << I.getFunction()->getName()
               << '\n'
               << I << "\n\n");

  for (unsigned Stage = MainF->getLastStage(); Stage--;) {
    MixContextTable T;
    auto ClearFunctionMapOnExit =
        make_scope_exit(std::bind(&decltype(FunctionMap)::clear, &FunctionMap));

    assert(FunctionMap.empty() && "FunctionMap is not empty");

    for (auto *F : Functions)
      FunctionMap[F] =
          declareFunction(*F, F == MainF ? GlobalValue::ExternalLinkage
                                         : GlobalValue::InternalLinkage);

    for (auto &P : FunctionMap)
      buildFunction(*P.first, P.second, T);

    MainF = buildMain(*FunctionMap.lookup(MainF).Mix, *MainF, T);
    Functions.push_back(MainF);
  }

  SmallVector<Value *, 4> Args;
  std::copy(std::next(I.arg_begin()), I.arg_end(), std::back_inserter(Args));

  // Insert call to the main function.
  B->SetInsertPoint(&I);
  Args.front() = B->CreateBitCast(
      Args.front(), getContextPtrTy(B->getContext()), Args.front()->getName());
  return B->CreateBitCast(B->CreateCall(MainF, Args, I.getName()), I.getType(),
                          I.getName());
}

namespace {

template <typename TypeOutputIt, typename NameOutputIt, typename AttrOutputIt,
          typename StagePredicate>
void getParamsByStage(const Function &F, TypeOutputIt OutType,
                      NameOutputIt OutName, AttrOutputIt OutAttr,
                      StagePredicate Pred) {
  AttributeList FA = F.getAttributes();

  for (const Argument &A : F.args()) {
    if (!Pred(A.getStage()))
      continue;

    *OutType++ = A.getType();
    *OutName++ = A.getName();
    *OutAttr++ = FA.getParamAttributes(A.getArgNo());
  }
}

struct Params {
  SmallVector<Type *, 4> Types;
  SmallVector<StringRef, 4> Names;
  SmallVector<AttributeSet, 4> Attrs;

  template <typename StagePredicate>
  Params(const Function &F, StagePredicate Pred, unsigned NumFrontReserved = 0)
      : Types(NumFrontReserved), Names(NumFrontReserved),
        Attrs(NumFrontReserved) {
    getParamsByStage(F, std::back_inserter(Types), std::back_inserter(Names),
                     std::back_inserter(Attrs), Pred);
  }

  // Set names and attributes of function arguments from this object.
  void applyTo(Function &F) const {
    assert(F.arg_size() == Types.size() && "Argument count mismatch");

    for (unsigned Num = 0; Num != F.arg_size(); ++Num) {
      F.arg_begin()[Num].setName(Names[Num]);
      F.addParamAttrs(Num, Attrs[Num]);
    }
  }
};

} // namespace

// Move all last-stage arguments to the next stage and return a function of
// all the other arguments and MixContext table pointer.
Mix::StagedFunctionInfo
Mix::declareFunction(Function &F, GlobalValue::LinkageTypes Linkage) const {
  LLVMContext &C = F.getContext();
  AttributeList FA = F.getAttributes();
  Params SP(F, std::bind(std::less<unsigned>(), _1, F.getLastStage()), 1);
  Params DP(F, std::bind(std::equal_to<unsigned>(), _1, F.getLastStage()));

  // Insert context parameter.
  SP.Types.front() = MixContextTable::getTablePtrTy(C);
  SP.Names.front() = "mix.context";
  {
    AttrBuilder AB;
    AB.addStageAttr(F.getLastStage() - 1);
    SP.Attrs.front() = AttributeSet::get(C, AB);
  }

  // When the source function has a static return, mix function returns a
  // struct of two members:
  //   - Return value
  //   - Pointer to the next-stage function
  Type *RetTy = F.getReturnStage() < F.getLastStage()
                    ? StructType::get(C, {F.getReturnType(), getValuePtrTy(C)})
                    : static_cast<Type *>(getValuePtrTy(C));

  // Declare function.
  Function *NewF =
      Function::Create(FunctionType::get(RetTy, SP.Types, false),
                       GlobalValue::PrivateLinkage, F.getName() + ".mix");
  M->getFunctionList().insertAfter(F.getIterator(), NewF);

  // Apply attributes.
  NewF->addAttributes(AttributeList::FunctionIndex,
                      FA.getAttributes(AttributeList::FunctionIndex));
  SP.applyTo(*NewF);

  return {NewF, FunctionType::get(F.getReturnType(), DP.Types, false), Linkage};
}

Function *Mix::buildMain(Function &F, Function &SourceF,
                         MixContextTable &T) const {
  LLVMContext &C = B->getContext();

  Params P(F, [](unsigned) { return true; });
  P.Types.front() = getContextPtrTy(C);
  P.Names.front() = "context";

  auto *MainF = Function::Create(
      FunctionType::get(getValuePtrTy(C), P.Types, false),
      GlobalValue::PrivateLinkage, SourceF.getName() + ".main");
  M->getFunctionList().insertAfter(SourceF.getIterator(), MainF);
  P.applyTo(*MainF);
  B->SetInsertPoint(BasicBlock::Create(C, "", MainF));

  // Build context table.
  Value *TP =
      T.build(*B, MainF->arg_begin(), SourceF.getName().str() + ".module");

  // Call the main mix function.
  SmallVector<Value *, 4> Args{TP};
  std::transform(std::next(MainF->arg_begin()), MainF->arg_end(),
                 std::back_inserter(Args), [](Argument &A) { return &A; });
  auto *S = B->CreateCall(&F, Args);
  auto *V = SourceF.getReturnStage() < SourceF.getLastStage()
                ? B->CreateExtractValue(S, 1)
                : S;

  // Dispose context table.
  MixContext(T, TP).dispose(*B);

  B->CreateRet(V);
  return MainF;
}

void Mix::buildFunction(Function &SourceF, const StagedFunctionInfo &SFI,
                        MixContextTable &T) {
  MixContext MC(T, SFI.Mix->arg_begin());
  SaveAndRestore<MixContext *> RestoreMCOnExit(this->MC, &MC);
  StagedIRBuilder<IRBuilder<>> SB(*B, MC);
  SaveAndRestore<StagedIRBuilder<IRBuilder<>> *> RestoreSBOnExit(this->SB, &SB);
  SaveAndRestore<Function *> RestoreSourceFOnExit(this->SourceF, &SourceF);
  Params P(SourceF,
           std::bind(std::equal_to<unsigned>(), _1, SourceF.getLastStage()));

  B->SetInsertPoint(BasicBlock::Create(B->getContext(), "", SFI.Mix));
  SB.createFunction(SFI.FTy, SFI.Linkage, SourceF.getName(), "function", true);

  // Stage function arguments.
  {
    unsigned StaticArgNum = 1; // 0th argument is context table pointer
    unsigned DynamicArgNum = 0;

    for (Argument &A : SourceF.args()) {
      if (A.getStage() < SourceF.getLastStage()) {
        SB.defineStatic(&A, &SFI.Mix->arg_begin()[StaticArgNum++]);
        SB.stageStatic(&A);
      } else {
        SB.setName(SB.stage(&A, DynamicArgNum), P.Names[DynamicArgNum]);
        DynamicArgNum += 1;
      }
    }
  }

  B->CreateBr(SB.defineStatic(&SourceF.getEntryBlock()));

  // Build static basic blocks in depth-first order.
  for (BasicBlock *BB : depth_first(&SourceF)) {
    if (BTA->getStage(BB) < SourceF.getLastStage())
      buildBasicBlock(*BB);
  }
}

namespace {

// Traverse dynamic blocks in the CFG starting from a given block and
// following dynamic control flow edges dynamic terminators.
//
// Outputs the starting block, then all dynamic blocks traversed, and then
// optionally the block with a static terminator (there must be at most one).
// Returns the static terminator if found.
template <typename OutputIt>
TerminatorInst *traverseDynamicBlocks(BasicBlock &Start, OutputIt Out,
                                      const BindingTimeAnalysis &BTA) {
  // The block with a static terminator reachable by dynamic edges from the
  // starting block.
  BasicBlock *Term = nullptr;

  for (auto B = df_begin(&Start); B != df_end(&Start);) {
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

} // namespace

void Mix::buildBasicBlock(BasicBlock &SourceBB) const {
  DEBUG(dbgs() << "Building static basic block ";
        SourceBB.printAsOperand(dbgs(), false); dbgs() << ":\n");

  B->SetInsertPoint(SB->defineStatic(&SourceBB));

  // Build dynamic terminator in the dynamic predecessor block.
  if (&SourceBB != &SourceF->getEntryBlock()) {
    SB->stage(std::unique_ptr<Instruction, ValueDeleter>(
        BranchInst::Create(&SourceBB)));
  }

  std::vector<BasicBlock *> Blocks;
  TerminatorInst *StaticTerm =
      traverseDynamicBlocks(SourceBB, std::back_inserter(Blocks), *BTA);

  for (auto *BB : Blocks) {
    DEBUG(dbgs() << "  "; BB->printAsOperand(dbgs(), false); dbgs() << '\n');

    SB->positionBuilderAtEnd(SB->stage(BB, true));

    std::for_each(BB->begin(), BB->end(),
                  std::bind(&Mix::buildInstruction, this, _1));
  }

  DEBUG(if (StaticTerm) dbgs() << *StaticTerm << "\n\n";
        else dbgs() << "  (no static terminator)\n\n");

  if (!StaticTerm)
    buildRetVoid();
  else if (auto *StaticRet = dyn_cast<ReturnInst>(StaticTerm))
    buildRet(*StaticRet->getReturnValue());
}

void Mix::buildInstruction(Instruction &I) const {
  if (auto *Call = dyn_cast<CallInst>(&I)) {
    if (buildCall(*Call))
      return;
  }

  if (BTA->getStage(&I) == SourceF->getLastStage() || isa<ReturnInst>(I)) {
    SB->stage(&I);
    return;
  }

  if (auto *Phi = dyn_cast<PHINode>(&I))
    SB->defineStatic(
        Phi, BTA->getPhiValueBindingTime(Phi) == SourceF->getLastStage());

  SB->stageStatic(&I);
}

// Build call instruction, returning false if the instruction must be general
// handled generically.
bool Mix::buildCall(CallInst &Call) const {
  if (!Call.getCalledFunction()) {
    assert(BTA->getStage(&Call) == SourceF->getLastStage() &&
           "Indirect calls cannot have static return values");
    return false;
  }

  Function &Callee = *Call.getCalledFunction();

  if (!Callee.isStaged()) {
    if (!Callee.hasExternalLinkage())
      return false;

    SB->setCalledValue(SB->stage(&Call),
                       SB->createFunction(Callee.getFunctionType(),
                                          Callee.getLinkage(), Callee.getName(),
                                          Callee.getName()));
    return true;
  }

  if (Callee.getLastStage() < SourceF->getLastStage())
    return false;

  const StagedFunctionInfo &CalleeInfo = FunctionMap.lookup(&Callee);
  SmallVector<Value *, 4> StaticArgs{MC->getTablePointer()};
  SmallVector<Value *, 4> DynamicArgs;

  // Split arguments by binding time.
  for (unsigned ArgNo = 0; ArgNo < Call.getNumArgOperands(); ++ArgNo) {
    Value *Arg = Call.getArgOperand(ArgNo);

    if (Callee.getParamStage(ArgNo) < SourceF->getLastStage()) {
      StaticArgs.push_back(SB->defineStatic(Arg));
    } else {
      DynamicArgs.push_back(Arg);
    }
  }

  // Build a static call.
  auto *S = B->CreateCall(CalleeInfo.Mix, StaticArgs, Call.getName());
  SB->positionBuilderAtEnd(SB->getBasicBlock());

  Instruction *StaticV, *DynCallee;

  if (Callee.getReturnStage() < SourceF->getLastStage()) {
    StaticV = cast<Instruction>(
        B->CreateExtractValue(S, 0, Call.getName()));
    DynCallee =
        cast<Instruction>(B->CreateExtractValue(S, 1, Call.getName() + ".res"));
  } else {
    StaticV = nullptr;
    DynCallee = S;
  }

  // Build a dynamic call.
  auto *DynamicV = SB->stage(std::unique_ptr<Instruction, ValueDeleter>(
      CallInst::Create(ConstantPointerNull::get(CalleeInfo.FTy->getPointerTo()),
                       DynamicArgs, Call.getName())));
  SB->setCalledValue(DynamicV, DynCallee);

  // Set staged value for the call.
  if (StaticV) {
    SB->defineStatic(&Call, StaticV);
    SB->stageStatic(&Call);
  } else {
    SB->setStagedValue(&Call, DynamicV);
  }

  return true;
}

// Build static value return from the mix function.
void Mix::buildRet(Value &V) const {
  std::array<Value *, 2> MRV = {SB->defineStatic(&V), SB->getFunction()};
  B->CreateAggregateRet(MRV.data(), MRV.size());
}

// Build static void return from the mix function.
void Mix::buildRetVoid() const {
  B->CreateRet(SB->getFunction());
}
