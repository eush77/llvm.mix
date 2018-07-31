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

#include "Types.h"

#include "llvm-c/Core.h"
#include "llvm/ADT/ArrayRef.h"
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
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>
#include <functional>
#include <string>
#include <unordered_map>
#include <utility>

namespace llvm {

namespace mix {

// This class provides a wrapper around IRBuilder to build code that builds
// other code when executed, using LLVM library API.
template <typename IRBuilder> class StagedIRBuilder {
public:
  StagedIRBuilder(IRBuilder &Builder, Value *StagedContext)
      : B(Builder), StagedContext(StagedContext) {}

  // Interface to particular LLVM API calls.
  Instruction *createModule(const Twine &ModuleId, const Twine &InstName = "");
  Instruction *createFunction(FunctionType *Type,
                              GlobalValue::LinkageTypes Linkage,
                              const Twine &Name = "",
                              Instruction *StagedModule = nullptr,
                              const Twine &InstName = "");
  Instruction *setLinkage(Value *Global, GlobalValue::LinkageTypes Linkage);
  Instruction *createBuilder(Instruction *StagedFunction,
                             const Twine &InstName = "");
  Instruction *positionBuilderAtEnd(Instruction *StagedBasicBlock,
                                    const Twine &InstName = "");
  Instruction *disposeBuilder(const Twine &InstName = "");

  // Stage a type by inserting commands to reconstruct it.
  Instruction *stage(Type *Ty, const Twine &InstName = "");

  // Stage a value by inserting commands to build it, unless it exists, in
  // which case return the previous value. Staging a value that is a basic
  // block means creating an empty basic block in the staged function.
  Instruction *stage(Value *V);

  // Stage a string by creating a global string variable with a given
  // initializer and returning an `i8*' pointer to the first character.
  Value *stage(StringRef Name, const Twine &VarName = "mix.name") {
    return B.CreateGlobalStringPtr(Name, VarName);
  }

private:
  IntegerType *getBoolTy() { return mix::getBoolTy(B.getContext()); }
  IntegerType *getIntPredicateTy() {
    return mix::getIntPredicateTy(B.getContext());
  }
  IntegerType *getLinkageTy() { return mix::getLinkageTy(B.getContext()); }
  IntegerType *getOpcodeTy() { return mix::getOpcodeTy(B.getContext()); }
  IntegerType *getUnsignedIntTy() {
    return mix::getUnsignedIntTy(B.getContext());
  }
  IntegerType *getUnsignedLongLongIntTy() {
    return mix::getUnsignedLongLongIntTy(B.getContext());
  }
  PointerType *getBasicBlockPtrTy() {
    return mix::getBasicBlockPtrTy(B.getContext());
  }
  PointerType *getBuilderPtrTy() {
    return mix::getBuilderPtrTy(B.getContext());
  }
  PointerType *getCharPtrTy() { return mix::getCharPtrTy(B.getContext()); }
  PointerType *getContextPtrTy() {
    return mix::getContextPtrTy(B.getContext());
  }
  PointerType *getModulePtrTy() { return mix::getModulePtrTy(B.getContext()); }
  PointerType *getTypePtrTy() { return mix::getTypePtrTy(B.getContext()); }
  PointerType *getValuePtrTy() { return mix::getValuePtrTy(B.getContext()); }

  Constant *getAPIFunction(const Twine &Name, Type *Result,
                           ArrayRef<Type *> Params) {
    return B.GetInsertBlock()->getModule()->getOrInsertFunction(
        Name.str(), FunctionType::get(Result, Params, false));
  }

  Instruction *stageArgument(Argument *Arg);
  Instruction *stageBasicBlock(BasicBlock *Block);
  Instruction *stageConstant(Constant *Const, const Twine &InstName = "");
  void stageIncomingList(PHINode *Phi, Instruction *StagedPhi);
  Instruction *stageInstruction(Instruction *Inst);

  // Register a callback to be called when the value is staged. If the value
  // has already been staged, call it immediately.
  void whenStaged(Value *V, std::function<void(Instruction *)> Callback);

  IRBuilder &B;
  Value *StagedContext;
  Instruction *StagedBuilder = nullptr;
  Instruction *StagedFunction = nullptr;
  DenseMap<Type *, Instruction *> StagedTypes;
  DenseMap<Value *, Instruction *> StagedValues;
  std::unordered_multimap<Value *, std::function<void(Instruction *)>>
      StageCallbacks;
};

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createModule(const Twine &ModuleId,
                                                      const Twine &InstName) {
  return B.CreateCall(
      getAPIFunction("LLVMModuleCreateWithNameInContext", getModulePtrTy(),
                     {getCharPtrTy(), getContextPtrTy()}),
      {stage(ModuleId.str()), StagedContext},
      InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createFunction(
    FunctionType *Type, GlobalValue::LinkageTypes Linkage, const Twine &Name,
    Instruction *StagedModule, const Twine &InstName) {
  auto *F = B.CreateCall(
      getAPIFunction("LLVMAddFunction", getValuePtrTy(),
                     {getModulePtrTy(), getCharPtrTy(), getTypePtrTy()}),
      {StagedModule, stage(Name.str()), stage(Type)},
      InstName);

  if (Linkage != GlobalValue::ExternalLinkage) {
    setLinkage(F, Linkage);
  }

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

  return B.CreateCall(getAPIFunction("LLVMSetLinkage", B.getVoidTy(),
                                     {getValuePtrTy(), getLinkageTy()}),
                      {Global, ConstantInt::get(getLinkageTy(), CAPILinkage)});
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createBuilder(Instruction *SF,
                                                       const Twine &InstName) {
  assert(!StagedBuilder && "Staged IRBuilder is already created");

  auto *SB =
      B.CreateCall(getAPIFunction("LLVMCreateBuilderInContext",
                                  getBuilderPtrTy(), {getContextPtrTy()}),
                   {StagedContext}, InstName);

  StagedBuilder = SB;
  StagedFunction = SF;
  return SB;
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::positionBuilderAtEnd(Instruction *StagedBlock,
                                                 const Twine &InstName) {
  return B.CreateCall(getAPIFunction("LLVMPositionBuilderAtEnd", B.getVoidTy(),
                                     {getBuilderPtrTy(), getBasicBlockPtrTy()}),
                      {StagedBuilder, StagedBlock}, InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::disposeBuilder(const Twine &InstName) {
  return B.CreateCall(
      getAPIFunction("LLVMDisposeBuilder", B.getVoidTy(), {getBuilderPtrTy()}),
      {StagedBuilder}, InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Type *Ty,
                                               const Twine &InstName) {
  Instruction *StagedTy = StagedTypes.lookup(Ty);

  if (StagedTy)
    return StagedTy;

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    StagedTy = B.CreateCall(getAPIFunction("LLVMVoidTypeInContext",
                                           getTypePtrTy(), {getContextPtrTy()}),
                            {StagedContext}, InstName);
    break;

  case Type::IntegerTyID: {
    unsigned BitWidth = cast<IntegerType>(Ty)->getBitWidth();

    switch (BitWidth) {
    case 1:
    case 8:
    case 16:
    case 32:
    case 64:
    case 128:
      StagedTy = B.CreateCall(
          getAPIFunction(Twine("LLVMInt") + std::to_string(BitWidth) +
                             "TypeInContext",
                         getTypePtrTy(), {getContextPtrTy()}),
          {StagedContext}, InstName);
      break;

    default:
      StagedTy = B.CreateCall(
          getAPIFunction("LLVMIntTypeInContext", getTypePtrTy(),
                         {getContextPtrTy(), getUnsignedIntTy()}),
          {StagedContext, ConstantInt::get(getUnsignedIntTy(), BitWidth)},
          InstName);
    }

    break;
  }

  case Type::FunctionTyID: {
    auto *FT = cast<FunctionType>(Ty);
    Value *Params;

    if (FT->getNumParams()) {
      Params = B.CreateAlloca(getTypePtrTy(), B.getInt32(FT->getNumParams()));

      for (unsigned ParamIndex = 0; ParamIndex < FT->getNumParams();
           ++ParamIndex) {
        B.CreateStore(stage(FT->getParamType(ParamIndex)),
                      B.CreateGEP(Params, B.getInt32(ParamIndex)));
      }
    } else {
      Params = ConstantPointerNull::get(PointerType::getUnqual(getTypePtrTy()));
    }

    StagedTy = B.CreateCall(
        getAPIFunction("LLVMFunctionType", getTypePtrTy(),
                       {getTypePtrTy(), PointerType::getUnqual(getTypePtrTy()),
                        getUnsignedIntTy(), getBoolTy()}),
        {stage(FT->getReturnType()), Params,
         ConstantInt::get(getUnsignedIntTy(), FT->getNumParams()),
         ConstantInt::get(getBoolTy(), false)},
        InstName);
    break;
  }
  }

  assert(StagedTy && "Unsupported type");

  StagedTypes[Ty] = StagedTy;
  return StagedTy;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageArgument(Argument *Arg) {
  return B.CreateCall(
      getAPIFunction("LLVMGetParam", getValuePtrTy(),
                     {getValuePtrTy(), getUnsignedIntTy()}),
      {StagedFunction, ConstantInt::get(getUnsignedIntTy(), Arg->getArgNo())},
      Arg->getName());
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageBasicBlock(BasicBlock *Block) {
  return B.CreateCall(
      getAPIFunction("LLVMAppendBasicBlockInContext", getBasicBlockPtrTy(),
                     {getContextPtrTy(), getValuePtrTy(), getCharPtrTy()}),
      {StagedContext, StagedFunction, stage(Block->getName())},
      Block->getName());
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageConstant(Constant *Const,
                                                       const Twine &InstName) {
  if (auto *ConstInt = dyn_cast<ConstantInt>(Const)) {
    assert(ConstInt->getValue().getNumWords() == 1 &&
           "Unsupported integer width");

    return B.CreateCall(
        getAPIFunction(
            "LLVMConstInt", getValuePtrTy(),
            {getTypePtrTy(), getUnsignedLongLongIntTy(), getBoolTy()}),
        {stage(Const->getType()),
         ConstantInt::get(getUnsignedLongLongIntTy(), ConstInt->getZExtValue()),
         ConstantInt::get(getBoolTy(), false)},
        InstName);
  }

  llvm_unreachable("Unsupported constant kind");
}

// Stage list of incoming values and add them to the already staged Phi node.
// This can't always happen right away because incoming values may not be
// ready yet, so add hooks to build the list when they will be.
template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageIncomingList(PHINode *Phi,
                                                   Instruction *StagedPhi) {
  auto *IncomingValueAlloca = B.CreateAlloca(getValuePtrTy(), B.getInt32(1));
  auto *IncomingBlockAlloca =
      B.CreateAlloca(getBasicBlockPtrTy(), B.getInt32(1));

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    whenStaged(Phi->getIncomingValue(IncomingNum),
               [=](Instruction *StagedIncomingValue) {
                 B.CreateStore(StagedIncomingValue, IncomingValueAlloca);
                 B.CreateStore(stage(Phi->getIncomingBlock(IncomingNum)),
                               IncomingBlockAlloca);

                 B.CreateCall(getAPIFunction(
                                  "LLVMAddIncoming", B.getVoidTy(),
                                  {getValuePtrTy(),
                                   PointerType::getUnqual(getValuePtrTy()),
                                   PointerType::getUnqual(getBasicBlockPtrTy()),
                                   getUnsignedIntTy()}),
                              {StagedPhi, IncomingValueAlloca,
                               IncomingBlockAlloca,
                               ConstantInt::get(getUnsignedIntTy(), 1)});
               });
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageInstruction(Instruction *Inst) {
  SmallVector<Type *, 4> ParamTypes{getBuilderPtrTy()};
  SmallVector<Value *, 4> Arguments{StagedBuilder};
  StringRef BuilderName;

  auto pushArg = [&ParamTypes, &Arguments](Type *Ty, Value *V) {
    assert(V->getType() == Ty && "Argument type mismatch");

    ParamTypes.push_back(Ty);
    Arguments.push_back(V);
  };

  auto setBuilderName = [&BuilderName](StringRef Name) {
    assert(BuilderName.empty() && "Builder name is already set");
    BuilderName = Name;
  };

  if (Inst->isBinaryOp()) {
    LLVMOpcode CAPIOpcode = LLVMGetInstructionOpcode(wrap(Inst));

    pushArg(getOpcodeTy(), ConstantInt::get(getOpcodeTy(), CAPIOpcode));
    pushArg(getValuePtrTy(), stage(Inst->getOperand(0)));
    pushArg(getValuePtrTy(), stage(Inst->getOperand(1)));
    pushArg(getCharPtrTy(), stage(Inst->getName()));
    setBuilderName("BinOp");
  } else {
    switch (Inst->getOpcode()) {
    case Instruction::Alloca: {
      auto *Alloca = cast<AllocaInst>(Inst);

      pushArg(getTypePtrTy(), stage(Alloca->getAllocatedType()));
      if (Alloca->isArrayAllocation()) {
        pushArg(getValuePtrTy(), stage(Alloca->getArraySize()));
        setBuilderName("ArrayAlloca");
      } else {
        setBuilderName("Alloca");
      }
      pushArg(getCharPtrTy(), stage(Inst->getName()));
      break;
    }

    case Instruction::Br: {
      auto *Br = cast<BranchInst>(Inst);

      if (Br->isConditional()) {
        pushArg(getValuePtrTy(), stage(Br->getCondition()));
        pushArg(getBasicBlockPtrTy(), stage(Br->getSuccessor(0)));
        pushArg(getBasicBlockPtrTy(), stage(Br->getSuccessor(1)));
        setBuilderName("CondBr");
      } else {
        pushArg(getBasicBlockPtrTy(), stage(Br->getSuccessor(0)));
        setBuilderName("Br");
      }
      break;
    }

    case Instruction::ICmp: {
      LLVMIntPredicate CAPIIntPredicate = LLVMGetICmpPredicate(wrap(Inst));

      pushArg(getIntPredicateTy(),
              ConstantInt::get(getIntPredicateTy(), CAPIIntPredicate));
      pushArg(getValuePtrTy(), stage(Inst->getOperand(0)));
      pushArg(getValuePtrTy(), stage(Inst->getOperand(1)));
      pushArg(getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("ICmp");
      break;
    }

    case Instruction::Load: {
      auto *Load = cast<LoadInst>(Inst);

      pushArg(getValuePtrTy(), stage(Load->getPointerOperand()));
      pushArg(getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Load");
      break;
    }

    case Instruction::PHI: {
      auto *Phi = cast<PHINode>(Inst);

      pushArg(getTypePtrTy(), stage(Phi->getType()));
      pushArg(getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Phi");
      break;
    }

    case Instruction::Store: {
      auto *Store = cast<StoreInst>(Inst);

      pushArg(getValuePtrTy(), stage(Store->getValueOperand()));
      pushArg(getValuePtrTy(), stage(Store->getPointerOperand()));
      setBuilderName("Store");
      break;
    }

    case Instruction::Ret: {
      auto *Ret = cast<ReturnInst>(Inst);

      if (auto *ReturnValue = Ret->getReturnValue()) {
        pushArg(getValuePtrTy(), stage(ReturnValue));
        setBuilderName("Ret");
      } else {
        setBuilderName("RetVoid");
      }
      break;
    }

    case Instruction::Trunc: {
      auto *Trunc = cast<TruncInst>(Inst);

      pushArg(getValuePtrTy(), stage(Trunc->getOperand(0)));
      pushArg(getTypePtrTy(), stage(Trunc->getDestTy()));
      pushArg(getCharPtrTy(), stage(Inst->getName()));
      setBuilderName("Trunc");
      break;
    }

    default:
      llvm_unreachable("Unsupported instruction");
    }
  }

  assert(!BuilderName.empty() && "Builder name is not set");

  return B.CreateCall(getAPIFunction(Twine("LLVMBuild") + BuilderName,
                                     getValuePtrTy(), ParamTypes),
                      Arguments, Inst->getName());
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Value *V) {
  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  if (auto *Arg = dyn_cast<Argument>(V)) {
    StagedV = stageArgument(Arg);
  } else if (auto *Block = dyn_cast<BasicBlock>(V)) {
    StagedV = stageBasicBlock(Block);
  } else if (auto *Const = dyn_cast<Constant>(V)) {
    StagedV = stageConstant(Const);
  } else if (auto *Inst = dyn_cast<Instruction>(V)) {
    StagedV = stageInstruction(Inst);

    if (auto *Phi = dyn_cast<PHINode>(Inst)) {
      stageIncomingList(Phi, StagedV);
    }
  }

  assert(StagedV && "Unsupported value kind");

  StagedValues[V] = StagedV;

  // Call stage callbacks for the value.
  for (auto &SCPair: make_range(StageCallbacks.equal_range(V))) {
    SCPair.second(StagedV);
  }
  StageCallbacks.erase(V);

  return StagedV;
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

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
