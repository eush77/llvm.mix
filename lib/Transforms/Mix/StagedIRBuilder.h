//===- StagedIRBuilder.h ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the helper class used to build LLVM IR at a later
// execution stage.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
#define LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H

#include "CAPIFunctions.h"
#include "MixContext.h"
#include "Types.h"

#include "llvm-c/Core.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

#include <cassert>
#include <functional>
#include <iterator>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std::string_literals;

namespace llvm {

namespace mix {

// This class provides a wrapper around IRBuilder to build code that builds
// other code when executed, using LLVM library API.
template <typename IRBuilder> class StagedIRBuilder {
public:
  StagedIRBuilder(IRBuilder &Builder, const MixContext &C) : B(Builder), C(C) {}
  StagedIRBuilder(const StagedIRBuilder &Other) = delete;
  StagedIRBuilder(StagedIRBuilder &&Other) = default;

  IRBuilder &getBuilder() const { return B; }
  MixContext &getContext() { return C; }

  // Interface to current staged function/block.
  Instruction *getFunction() const { return SF; }
  Instruction *getBasicBlock() const { return SBB; }

  // Interface to particular LLVM API calls.
  Instruction *createFunction(FunctionType *Type,
                              GlobalValue::LinkageTypes Linkage,
                              const Twine &Name = "",
                              const Twine &InstName = "",
                              bool SetFunction = false);
  Instruction *setLinkage(Value *Global, GlobalValue::LinkageTypes Linkage);
  Instruction *setName(Instruction *I, StringRef Name);
  Instruction *positionBuilderAtEnd(Instruction *StagedBasicBlock,
                                    const Twine &InstName = "");

  // Stage a value by inserting commands to build it, unless it exists, in
  // which case return the previous value. Staging a value that is a basic
  // block means creating an empty basic block in the staged function.
  Instruction *stage(Value *V);
  Instruction *stage(Argument *A, unsigned ArgNo);
  Instruction *stage(BasicBlock *BB, bool SetBasicBlock);
  Instruction *stage(std::unique_ptr<Instruction, ValueDeleter> &&I) {
    return stageInstruction(I.get());
  }

  void setStagedValue(Value *V, Instruction *StagedV);

  // Define static value in the generated function if it is defined elsewhere.
  Value *defineStatic(Value *V);
  Value *defineStatic(Argument *A, Value *SA);
  BasicBlock *defineStatic(BasicBlock *BB);
  Instruction *defineStatic(Instruction *I, Instruction *V = nullptr);
  PHINode *defineStatic(PHINode *Phi, bool Staged);

  // Stage a static value as a constant in generated code.
  Instruction *stageStatic(Value *V);

  // Stage a string by creating a global string variable with a given
  // initializer and returning an `i8*' pointer to the first character.
  Value *stage(StringRef Name, const Twine &VarName = "mix.name") {
    return B.CreateGlobalStringPtr(Name, VarName);
  }

  // Add incoming value for a static phi.
  void addIncoming(PHINode *, Value *, BasicBlock *IncomingBB,
                   BasicBlock *StaticIncomingBB, bool Staged);

  // Change operands of dynamic instructions
  void setCalledValue(Instruction *Call, Instruction *V);
  void setCastedValue(Instruction *Cast, Instruction *V);

private:
  Module &getModule() const { return *B.GetInsertBlock()->getModule(); }
  Instruction *stageBasicBlock(BasicBlock *Block);
  void stageIncomingList(PHINode *Phi, Instruction *StagedPhi);
  Instruction *stageInstruction(Instruction *Inst);
  void stageSwitchCases(Instruction *StagedInst, SwitchInst *);
  void stageInstructionMetadata(Instruction *StagedInst, Instruction *Inst);
  Instruction *stageMetadata(Metadata *);
  Instruction *stageMDNode(MDNode *);

  // Register a callback to be called when the value is staged. If the value
  // has already been staged, call it immediately.
  void whenStaged(Value *V, std::function<void(Instruction *)> Callback);

  IRBuilder &B;
  MixContext C;
  Instruction *SF = nullptr;
  Instruction *SBB = nullptr;
  DenseMap<Value *, Instruction *> StagedValues;
  std::unordered_multimap<Value *, std::function<void(Instruction *)>>
      StageCallbacks;
  DenseMap<Argument *, Value *> StaticArguments;
  DenseMap<BasicBlock *, BasicBlock *> StaticBasicBlocks;
  DenseMap<Instruction *, Instruction *> StaticInstructions;
};

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createFunction(
    FunctionType *Type, GlobalValue::LinkageTypes Linkage, const Twine &Name,
    const Twine &InstName, bool SetFunction) {
  auto *F = B.CreateCall(getAddFunctionFn(getModule()),
                         {C.getModule(), stage(Name.str()), C.getType(Type)},
                         InstName);

  if (Linkage != GlobalValue::ExternalLinkage) {
    setLinkage(F, Linkage);
  }

  if (SetFunction)
    SF = F;

  return F;
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::setLinkage(Value *Global,
                                       GlobalValue::LinkageTypes Linkage) {
  LLVMLinkage CAPILinkage;

  switch (Linkage) {
  case GlobalValue::ExternalLinkage:
    CAPILinkage = LLVMExternalLinkage;
    break;

  case GlobalValue::AvailableExternallyLinkage:
    CAPILinkage = LLVMAvailableExternallyLinkage;
    break;

  case GlobalValue::LinkOnceAnyLinkage:
    CAPILinkage = LLVMLinkOnceAnyLinkage;
    break;

  case GlobalValue::LinkOnceODRLinkage:
    CAPILinkage = LLVMLinkOnceODRLinkage;
    break;

  case GlobalValue::WeakAnyLinkage:
    CAPILinkage = LLVMWeakAnyLinkage;
    break;

  case GlobalValue::WeakODRLinkage:
    CAPILinkage = LLVMWeakODRLinkage;
    break;

  case GlobalValue::AppendingLinkage:
    CAPILinkage = LLVMAppendingLinkage;
    break;

  case GlobalValue::InternalLinkage:
    CAPILinkage = LLVMInternalLinkage;
    break;

  case GlobalValue::PrivateLinkage:
    CAPILinkage = LLVMPrivateLinkage;
    break;

  case GlobalValue::ExternalWeakLinkage:
    CAPILinkage = LLVMExternalWeakLinkage;
    break;

  case GlobalValue::CommonLinkage:
    CAPILinkage = LLVMCommonLinkage;
    break;
  }

  return B.CreateCall(
      getSetLinkageFn(getModule()),
      {Global, ConstantInt::get(getLinkageTy(B.getContext()), CAPILinkage)});
}

// Set name of a staged value.
template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::setName(Instruction *I,
                                                 StringRef Name) {
  return B.CreateCall(getSetValueNameFn(getModule()), {I, stage(Name)});
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::positionBuilderAtEnd(Instruction *StagedBlock,
                                                 const Twine &InstName) {
  return B.CreateCall(getPositionBuilderAtEndFn(getModule()),
                      {C.getBuilder(), StagedBlock}, InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Argument *Arg, unsigned ArgNo) {
  assert(!StagedValues.count(Arg) && "Argument has already been staged");

  Instruction *StagedArg = B.CreateCall(
      getGetParamFn(getModule()),
      {SF, ConstantInt::get(getUnsignedIntTy(B.getContext()), ArgNo)},
      Arg->getName());

  setStagedValue(Arg, StagedArg);
  return StagedArg;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(BasicBlock *BB,
                                               bool SetBasicBlock) {
  Instruction *SBB = stage(BB);

  if (SetBasicBlock)
    this->SBB = SBB;

  return SBB;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageBasicBlock(BasicBlock *Block) {
  return B.CreateCall(getAppendBasicBlockInContextFn(getModule()),
                      {C.getContext(), SF, stage(Block->getName())},
                      Block->getName());
}

// Stage list of incoming values and add them to the already staged Phi node.
// This can't always happen right away because incoming values may not be
// ready yet, so add hooks to build the list when they will be.
template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageIncomingList(PHINode *Phi,
                                                   Instruction *StagedPhi) {
  auto *IncomingValueAlloca =
      B.CreateAlloca(getValuePtrTy(B.getContext()), B.getInt32(1));
  auto *IncomingBlockAlloca =
      B.CreateAlloca(getBasicBlockPtrTy(B.getContext()), B.getInt32(1));

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    whenStaged(Phi->getIncomingValue(IncomingNum),
               [=](Instruction *StagedIncomingValue) {
                 B.CreateStore(StagedIncomingValue, IncomingValueAlloca);
                 B.CreateStore(stage(Phi->getIncomingBlock(IncomingNum)),
                               IncomingBlockAlloca);

                 B.CreateCall(
                     getAddIncomingFn(getModule()),
                     {StagedPhi, IncomingValueAlloca, IncomingBlockAlloca,
                      ConstantInt::get(getUnsignedIntTy(B.getContext()), 1)});
               });
  }
}

namespace detail {

// Helper class for staging instructions
template <typename IRBuilder>
class StagedInstructionBuilder
    : public InstVisitor<StagedInstructionBuilder<IRBuilder>> {
public:
  explicit StagedInstructionBuilder(StagedIRBuilder<IRBuilder> &SB)
      : SB(SB), B(SB.getBuilder()), C(SB.getContext()),
        ParamTypes{getBuilderPtrTy(B.getContext())}, Arguments{C.getBuilder()} {
  }

  Instruction *stage(Instruction *);
  void visitAllocaInst(AllocaInst &);
  void visitBinaryOperator(BinaryOperator &);
  void visitBranchInst(BranchInst &);
  void visitCallInst(CallInst &);
  void visitCastInst(CastInst &);
  void visitExtractValueInst(ExtractValueInst &);
  void visitGetElementPtrInst(GetElementPtrInst &);
  void visitICmpInst(ICmpInst &);
  void visitInsertValueInst(InsertValueInst &);
  void visitLoadInst(LoadInst &);
  void visitPHINode(PHINode &);
  void visitReturnInst(ReturnInst &);
  void visitStoreInst(StoreInst &);
  void visitSwitchInst(SwitchInst &);
  void visitUnreachableInst(UnreachableInst &);

  // Report error on unsupported instruction
  void visitInstruction(Instruction &I) {
    llvm_unreachable("Unsupported instruction");
  }

private:
  void pushArg(Type *Ty, Value *V) {
    assert(V->getType() == Ty && "Argument type mismatch");

    ParamTypes.push_back(Ty);
    Arguments.push_back(V);
  }

  void setBuilderName(const char *Name) {
    assert(!BuilderName && "Builder name is already set");
    BuilderName = Name;
  }

  StagedIRBuilder<IRBuilder> &SB;
  IRBuilder &B;
  MixContext &C;
  std::vector<Type *> ParamTypes;
  std::vector<Value *> Arguments;
  const char *BuilderName = nullptr;
};

} // namespace detail

template <typename IRBuilder>
Instruction *
detail::StagedInstructionBuilder<IRBuilder>::stage(Instruction *I) {
  Module &M = *B.GetInsertBlock()->getModule();

  this->visit(I);
  assert(BuilderName && "Builder name is not set");

  return B.CreateCall(
      M.getOrInsertFunction(
          "LLVMBuild"s + BuilderName,
          FunctionType::get(getValuePtrTy(B.getContext()), ParamTypes, false)),
      Arguments, I->getName());
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitAllocaInst(
    AllocaInst &I) {
  pushArg(getTypePtrTy(B.getContext()), C.getType(I.getAllocatedType()));

  if (I.isArrayAllocation()) {
    pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getArraySize()));
    setBuilderName("ArrayAlloca");
  } else {
    setBuilderName("Alloca");
  }

  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitBinaryOperator(
    BinaryOperator &I) {
  LLVMOpcode CAPIOpcode = LLVMGetInstructionOpcode(wrap(&I));

  pushArg(getOpcodeTy(B.getContext()),
          ConstantInt::get(getOpcodeTy(B.getContext()), CAPIOpcode));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getOperand(0)));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getOperand(1)));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("BinOp");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitBranchInst(
    BranchInst &I) {
  if (I.isUnconditional()) {
    pushArg(getBasicBlockPtrTy(B.getContext()), SB.stage(I.getSuccessor(0)));
    setBuilderName("Br");
    return;
  }

  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getCondition()));
  pushArg(getBasicBlockPtrTy(B.getContext()), SB.stage(I.getSuccessor(0)));
  pushArg(getBasicBlockPtrTy(B.getContext()), SB.stage(I.getSuccessor(1)));
  setBuilderName("CondBr");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitCallInst(CallInst &I) {
  Value *Args = B.CreateAlloca(getValuePtrTy(B.getContext()),
                               B.getInt32(I.getNumArgOperands()));

  for (unsigned ArgNum = 0; ArgNum < I.getNumArgOperands(); ++ArgNum)
    B.CreateStore(SB.stage(I.getArgOperand(ArgNum)),
                  B.CreateGEP(Args, B.getInt32(ArgNum)));

  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getCalledValue()));
  pushArg(getValuePtrTy(B.getContext())->getPointerTo(), Args);
  pushArg(getUnsignedIntTy(B.getContext()),
          ConstantInt::get(getUnsignedIntTy(B.getContext()),
                           I.getNumArgOperands()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("Call");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitCastInst(CastInst &I) {
  LLVMOpcode CAPIOpcode = LLVMGetInstructionOpcode(wrap(&I));

  pushArg(getOpcodeTy(B.getContext()),
          ConstantInt::get(getOpcodeTy(B.getContext()), CAPIOpcode));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getOperand(0)));
  pushArg(getTypePtrTy(B.getContext()), C.getType(I.getType()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("Cast");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitExtractValueInst(
    ExtractValueInst &I) {
  assert(I.getNumIndices() == 1 &&
         "Unsupported number of indices for extractvalue");

  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getAggregateOperand()));
  pushArg(getUnsignedIntTy(B.getContext()),
          ConstantInt::get(getUnsignedIntTy(B.getContext()),
                           I.getIndices().front()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("ExtractValue");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitGetElementPtrInst(
    GetElementPtrInst &I) {
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getPointerOperand()));

  if (I.isInBounds() && I.getNumIndices() == 2 && I.hasAllConstantIndices() &&
      cast<ConstantInt>(I.idx_begin())->isZero()) {
    pushArg(
        getUnsignedIntTy(B.getContext()),
        ConstantExpr::getIntegerCast(cast<Constant>(*std::next(I.idx_begin())),
                                     getUnsignedIntTy(B.getContext()), true));
    pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
    setBuilderName("StructGEP");
    return;
  }

  Value *Indices = B.CreateAlloca(getValuePtrTy(B.getContext()),
                                  B.getInt32(I.getNumIndices()));
  auto IIter = I.idx_begin();

  for (unsigned INum = 0; INum < I.getNumIndices(); ++INum)
    B.CreateStore(SB.stage(*IIter++), B.CreateGEP(Indices, B.getInt32(INum)));

  pushArg(getValuePtrTy(B.getContext())->getPointerTo(), Indices);
  pushArg(
      getUnsignedIntTy(B.getContext()),
      ConstantInt::get(getUnsignedIntTy(B.getContext()), I.getNumIndices()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName(I.isInBounds() ? "InBoundsGEP" : "GEP");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitICmpInst(ICmpInst &I) {
  LLVMIntPredicate CAPIIntPredicate = LLVMGetICmpPredicate(wrap(&I));

  pushArg(
      getIntPredicateTy(B.getContext()),
      ConstantInt::get(getIntPredicateTy(B.getContext()), CAPIIntPredicate));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getOperand(0)));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getOperand(1)));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("ICmp");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitInsertValueInst(
    InsertValueInst &I) {
  assert(I.getNumIndices() == 1 &&
         "Unsupported number of indices for insertvalue");

  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getAggregateOperand()));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getInsertedValueOperand()));
  pushArg(getUnsignedIntTy(B.getContext()),
          ConstantInt::get(getUnsignedIntTy(B.getContext()),
                           I.getIndices().front()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("InsertValue");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitLoadInst(LoadInst &I) {
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getPointerOperand()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("Load");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitPHINode(PHINode &I) {
  pushArg(getTypePtrTy(B.getContext()), C.getType(I.getType()));
  pushArg(getCharPtrTy(B.getContext()), SB.stage(I.getName()));
  setBuilderName("Phi");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitReturnInst(
    ReturnInst &I) {
  if (!I.getReturnValue()) {
    setBuilderName("RetVoid");
    return;
  }

  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getReturnValue()));
  setBuilderName("Ret");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitStoreInst(StoreInst &I) {
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getValueOperand()));
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getPointerOperand()));
  setBuilderName("Store");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitSwitchInst(
    SwitchInst &I) {
  pushArg(getValuePtrTy(B.getContext()), SB.stage(I.getCondition()));
  pushArg(getBasicBlockPtrTy(B.getContext()), SB.stage(I.getDefaultDest()));
  pushArg(getUnsignedIntTy(B.getContext()),
          ConstantInt::get(getUnsignedIntTy(B.getContext()), I.getNumCases()));
  setBuilderName("Switch");
}

template <typename IRBuilder>
void detail::StagedInstructionBuilder<IRBuilder>::visitUnreachableInst(
    UnreachableInst &I) {
  setBuilderName("Unreachable");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageInstruction(Instruction *I) {
  Instruction *SI = detail::StagedInstructionBuilder<IRBuilder>(*this).stage(I);

  if (auto *Switch = dyn_cast<SwitchInst>(I))
    stageSwitchCases(SI, Switch);

  stageInstructionMetadata(SI, I);
  return SI;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageSwitchCases(Instruction *StagedInst,
                                                  SwitchInst *I) {
  for (const SwitchInst::CaseHandle &Case : I->cases())
    B.CreateCall(getAddCaseFn(getModule()),
                 {StagedInst, stage(Case.getCaseValue()),
                  stage(Case.getCaseSuccessor())});
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageInstructionMetadata(
    Instruction *StagedInst, Instruction *Inst) {
  SmallVector<std::pair<unsigned, MDNode *>, 2> MDs;
  Inst->getAllMetadata(MDs);

  for (auto &P : MDs) {
    unsigned KindID;
    MDNode *MDN;
    std::tie(KindID, MDN) = P;

    B.CreateCall(getSetMetadataFn(getModule()),
                 {StagedInst, C.getMDKindID(KindID), stageMetadata(MDN)});
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageMetadata(Metadata *MD) {
  switch (MD->getMetadataID()) {
  case Metadata::ConstantAsMetadataKind:
  case Metadata::LocalAsMetadataKind: {
    auto *V = cast<ValueAsMetadata>(MD)->getValue();
    Value *MD = B.CreateCall(getValueAsMetadataFn(getModule()), {stage(V)});

    return B.CreateCall(getMetadataAsValueFn(getModule()),
                        {C.getContext(), MD});
  }

  case Metadata::MDStringKind: {
    auto *MDS = cast<MDString>(MD);

    return B.CreateCall(
        getMDStringInContextFn(getModule()),
        {C.getContext(), stage(MDS->getString()),
         ConstantInt::get(getUnsignedIntTy(B.getContext()), MDS->getLength())});
  }

  case Metadata::MDTupleKind:
    return stageMDNode(cast<MDNode>(MD));
  }

  llvm_unreachable("Unhandled metadata kind");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageMDNode(MDNode *MDN) {
  Value *Vals = B.CreateAlloca(getValuePtrTy(B.getContext()),
                               B.getInt32(MDN->getNumOperands()));

  for (unsigned OpNum = 0; OpNum < MDN->getNumOperands(); ++OpNum) {
    B.CreateStore(stageMetadata(MDN->getOperand(OpNum).get()),
                  B.CreateGEP(Vals, B.getInt32(OpNum)));
  }

  return B.CreateCall(getMDNodeInContextFn(getModule()),
                      {C.getContext(), Vals,
                       ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                        MDN->getNumOperands())});
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Value *V) {
  if (isa<Constant>(V))
    return stageStatic(V);

  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  if (auto *Block = dyn_cast<BasicBlock>(V)) {
    StagedV = stageBasicBlock(Block);
  } else if (auto *Inst = dyn_cast<Instruction>(V)) {
    StagedV = stageInstruction(Inst);

    if (auto *Phi = dyn_cast<PHINode>(Inst)) {
      stageIncomingList(Phi, StagedV);
    }
  } else if (auto *MDV = dyn_cast<MetadataAsValue>(V)) {
    StagedV = stageMetadata(MDV->getMetadata());
  }

  assert(StagedV && "Unsupported value kind");
  setStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Argument *A, Value *SA) {
  if (A->getParent() == B.GetInsertBlock()->getParent())
    return A;

  if (SA) {
    assert(!StaticArguments.lookup(A) && "Argument has already been defined");
  } else {
    SA = StaticArguments.lookup(A);
    assert(SA && "Argument has not been defined");
    return SA;
  }

  return StaticArguments[A] = SA;
}

template <typename IRBuilder>
BasicBlock *StagedIRBuilder<IRBuilder>::defineStatic(BasicBlock *BB) {
  if (BB->getParent() == B.GetInsertBlock()->getParent())
    return BB;

  auto *SBB = StaticBasicBlocks.lookup(BB);

  if (SBB)
    return SBB;

  SBB = BasicBlock::Create(B.getContext(), BB->getName(),
                           B.GetInsertBlock()->getParent());
  return StaticBasicBlocks[BB] = SBB;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::defineStatic(Instruction *I,
                                                      Instruction *V) {
  assert((!V || I->getType() == V->getType()) && "Type mismatch");

  if (I->getFunction() == B.GetInsertBlock()->getParent()) {
    assert(!V && "Cannot define this instruction");
    return I;
  }

  auto *SI = StaticInstructions.lookup(I);

  if (SI) {
    assert(!V && "Instruction is already defined");
    return SI;
  }

  if (V) {
    return StaticInstructions[I] = V;
  }

  assert(!isa<PHINode>(I) && "Static PHI nodes require an explicit type");

  SI = B.Insert(I->clone(), I->getName());

  // Remap operands.
  for (Use &Op : SI->operands()) {
    Op = defineStatic(Op);
  }

  return StaticInstructions[I] = SI;
}

// Static phi has either current or staged type depending on the binding type
// of its operands.
template <typename IRBuilder>
PHINode *StagedIRBuilder<IRBuilder>::defineStatic(PHINode *Phi, bool Staged) {
  if (Phi->getFunction() == B.GetInsertBlock()->getParent())
    return Phi;

  auto *SPhi = cast_or_null<PHINode>(StaticInstructions.lookup(Phi));

  if (SPhi)
    return SPhi;

  Type *Ty = Staged ? getValuePtrTy(B.getContext()) : Phi->getType();
  SPhi = B.CreatePHI(Ty, Phi->getNumIncomingValues(), Phi->getName());
  SPhi->moveBefore(B.GetInsertBlock()->getFirstNonPHI());
  StaticInstructions[Phi] = SPhi;
  return SPhi;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::addIncoming(PHINode *Phi, Value *IncomingV,
                                             BasicBlock *IncomingBB,
                                             BasicBlock *StaticIncomingBB,
                                             bool Staged) {
  if (auto *IncomingI = dyn_cast<Instruction>(IncomingV)) {
    whenStaged(IncomingI, [=](Instruction *StagedI) {
      Value *IncomingV =
          Staged ? StagedI : this->StaticInstructions.lookup(IncomingI);

      Phi->addIncoming(IncomingV, defineStatic(StaticIncomingBB));
    });
    return;
  }

  if (!Staged) {
    Phi->addIncoming(defineStatic(IncomingV), defineStatic(StaticIncomingBB));
    return;
  }

  typename IRBuilder::InsertPoint IP = B.saveIP();

  // Staged value must be live on the edge from the incoming block, so stage
  // it at the end of the predecessor block.
  if (auto *Term = defineStatic(IncomingBB)->getTerminator())
    B.SetInsertPoint(Term);
  else
    B.SetInsertPoint(defineStatic(IncomingBB));

  Phi->addIncoming(stage(IncomingV), defineStatic(StaticIncomingBB));
  B.restoreIP(IP);
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Value *V) {
  if (isa<Constant>(V))
    return V;

  if (auto *A = dyn_cast<Argument>(V)) {
    return defineStatic(A, nullptr);
  } else if (auto *BB = dyn_cast<BasicBlock>(V)) {
    return defineStatic(BB);
  } else if (auto *I = dyn_cast<Instruction>(V)) {
    return defineStatic(I);
  }

  llvm_unreachable("Unsupported value kind");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageStatic(Value *V) {
  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  Value *StaticV = defineStatic(V);

  if (auto *StaticPhi = dyn_cast<PHINode>(StaticV)) {
    setStagedValue(V, StaticPhi);
    return StaticPhi;
  }

  if (isa<UndefValue>(StaticV)) {
    StagedV = B.CreateCall(getGetUndefFn(getModule()),
                           {C.getType(StaticV->getType())}, StaticV->getName());
    setStagedValue(V, StagedV);
    return StagedV;
  }

  switch (StaticV->getType()->getTypeID()) {
  case Type::VoidTyID: {
    StagedV = nullptr;          // No staged value
    break;
  }

  case Type::HalfTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID: {
    Type *Ty = StaticV->getType();
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();

    if (BitWidth <= getDoubleTy(B.getContext())->getPrimitiveSizeInBits()) {
      Value *Double =
          BitWidth < getDoubleTy(B.getContext())->getPrimitiveSizeInBits()
              ? B.CreateFPExt(StaticV, getDoubleTy(B.getContext()))
              : StaticV;
      StagedV = B.CreateCall(getConstRealFn(getModule()),
                             {C.getType(Ty), Double}, StaticV->getName());
    } else {
      Value *Int = stageStatic(B.CreateBitCast(StaticV, B.getIntNTy(BitWidth)));
      StagedV = B.CreateCall(getConstBitCastFn(getModule()),
                             {Int, C.getType(Ty)}, StaticV->getName());
    }
    break;
  }

  case Type::IntegerTyID: {
    IntegerType *Ty = cast<IntegerType>(StaticV->getType());
    unsigned BitWidth = Ty->getBitWidth();

    if (BitWidth <= getUnsignedLongLongIntTy(B.getContext())->getBitWidth()) {
      Value *ULL =
          BitWidth < getUnsignedLongLongIntTy(B.getContext())->getBitWidth()
              ? B.CreateZExt(StaticV, getUnsignedLongLongIntTy(B.getContext()))
              : StaticV;
      StagedV =
          B.CreateCall(getConstIntFn(getModule()),
                       {C.getType(Ty), ULL,
                        ConstantInt::get(getBoolTy(B.getContext()), false)},
                       StaticV->getName());
    } else {
      unsigned NumWords = alignTo(BitWidth, 64) / 64;
      Value *Words = B.CreateAlloca(B.getInt64Ty(), B.getInt32(NumWords));
      Value *Val = StaticV;

      for (unsigned WN = 0; WN < NumWords; ++WN) {
        B.CreateStore(B.CreateTrunc(Val, B.getInt64Ty()),
                      B.CreateGEP(Words, B.getInt32(WN)));
        Val = B.CreateLShr(Val, ConstantInt::get(Ty, 64));
      }

      StagedV = B.CreateCall(
          getConstIntOfArbitraryPrecisionFn(getModule()),
          {C.getType(Ty),
           ConstantInt::get(getUnsignedIntTy(B.getContext()), NumWords), Words},
          StaticV->getName());
    }
    break;
  }

  case Type::PointerTyID: {
    PointerType *Ty = cast<PointerType>(StaticV->getType());

    // Declare external functions instead of taking their address.
    // This is required for intrinsic calls to work, and otherwise makes for
    // more comprehensible code.
    if (Ty->getElementType()->isFunctionTy()) {
      if (auto *F = dyn_cast<Function>(StaticV->stripPointerCasts())) {
        if (F->hasExternalLinkage()) {
          StagedV = C.getFunction(F);

          if (StaticV->getType() != F->getType())
            StagedV = B.CreateCall(getBuildPointerCastFn(getModule()),
                                   {C.getBuilder(), StagedV,
                                    C.getType(StaticV->getType()),
                                    stage(StaticV->getName())});
          break;
        }
      }
    }

    Value *IntPtr =
        B.CreatePtrToInt(StaticV, getUnsignedLongLongIntTy(B.getContext()));

    StagedV = B.CreateCall(
        getConstIntToPtrFn(getModule()),
        {B.CreateCall(getConstIntFn(getModule()),
                      {C.getType(IntPtr->getType()), IntPtr,
                       ConstantInt::get(getBoolTy(B.getContext()), false)}),
         C.getType(Ty)},
        StaticV->getName());
    break;
  }

  case Type::StructTyID: {
    StructType *ST = cast<StructType>(StaticV->getType());
    Value *Elements;

    if (ST->getNumElements()) {
      Elements = B.CreateAlloca(getValuePtrTy(B.getContext()),
                                B.getInt32(ST->getNumElements()));

      for (unsigned ElNum = 0; ElNum < ST->getNumElements(); ++ElNum) {
        B.CreateStore(stageStatic(B.CreateExtractValue(StaticV, ElNum)),
                      B.CreateGEP(Elements, B.getInt32(ElNum)));
      }
    } else {
      Elements = ConstantPointerNull::get(
          PointerType::getUnqual(getValuePtrTy(B.getContext())));
    }

    if (ST->hasName()) {
      StagedV = B.CreateCall(getConstNamedStructFn(getModule()),
                             {C.getType(ST), Elements,
                              ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                               ST->getNumElements())});
    } else {
      StagedV = B.CreateCall(
          getConstStructInContextFn(getModule()),
          {C.getContext(), Elements,
           ConstantInt::get(getUnsignedIntTy(B.getContext()),
                            ST->getNumElements()),
           ConstantInt::get(getBoolTy(B.getContext()), ST->isPacked())});
    }
    break;
  }
  }

  assert((StagedV || StaticV->getType()->isVoidTy()) &&
         "Unsupported static value");
  setStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setStagedValue(Value *V,
                                                Instruction *StagedV) {
  assert(!StagedValues.count(V) && "Value is already staged");

  // Call stage callbacks for the value.
  for (auto &SCPair: make_range(StageCallbacks.equal_range(V))) {
    SCPair.second(StagedV);
  }
  StageCallbacks.erase(V);

  // The same Constant or MetadataAsValue can be used in a set of basic blocks
  // in which no single block dominates the rest, so reusing its staged value
  // is not safe because it can lead to the "Instruction does not dominate all
  // uses" Verifier error.
  if (!isa<Constant>(V) && !isa<MetadataAsValue>(V))
    StagedValues[V] = StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::whenStaged(
    Value *V, std::function<void(Instruction *)> Callback) {
  if (Instruction *StagedV = StagedValues.lookup(V)) {
    Callback(StagedV);
  } else {
    StageCallbacks.insert(std::make_pair(V, Callback));
  }
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setCalledValue(Instruction *I,
                                                Instruction *V) {
  auto *Call = cast<CallInst>(I);
  assert(Call->getCalledFunction()->getName() == "LLVMBuildCall" &&
         "Not a dynamic call");

  Call->setArgOperand(1, V);
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setCastedValue(Instruction *I,
                                                Instruction *V) {
  auto *Call = cast<CallInst>(I);
  assert(Call->getCalledFunction()->getName() == "LLVMBuildCast" &&
         "Not a dynamic cast");

  Call->setArgOperand(2, V);
}

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
