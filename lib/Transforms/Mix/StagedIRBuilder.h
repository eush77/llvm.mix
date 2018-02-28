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

#include "llvm-c/Core.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>

namespace llvm {

// This class provides a wrapper around IRBuilder to build code that builds
// other code when executed, using LLVM library API.
template <typename IRBuilder> class StagedIRBuilder {
public:
  StagedIRBuilder(IRBuilder &Builder, Value *StagedContext)
      : B(Builder), StagedContext(StagedContext) {}

  ~StagedIRBuilder() {
    assert(!StagedBuilder && "Staged IRBuilder is not disposed");
  }

  // Interface to particular LLVM API calls.
  Instruction *createModule(const Twine &ModuleId, const Twine &InstName = "");
  Instruction *createFunction(FunctionType *Type,
                              GlobalValue::LinkageTypes Linkage,
                              const Twine &Name = "",
                              Value *StagedModule = nullptr,
                              const Twine &InstName = "");
  Instruction *createBasicBlock(const Twine &Name = "",
                                Value *StagedFunction = nullptr,
                                const Twine &InstName = "");
  Instruction *setLinkage(Value *Global, GlobalValue::LinkageTypes Linkage);
  Instruction *createBuilder(const Twine &InstName = "");
  Instruction *positionBuilderAtEnd(Value *Block, const Twine &InstName = "");
  Instruction *disposeBuilder(const Twine &InstName = "");

  // Stage a type by inserting commands to reconstruct it.
  Instruction *createType(Type *Ty, const Twine &InstName = "");

  // Stage a new instruction by inserting commands to build it.
  Instruction *createInstruction(Instruction *Inst);

private:
  constexpr static const char *GlobalPrefix = "mix.name";

  Type *getContextPtrTy() { return B.getInt8PtrTy(); }
  Type *getModulePtrTy() { return B.getInt8PtrTy(); }
  Type *getFunctionPtrTy() { return B.getInt8PtrTy(); }
  Type *getBasicBlockPtrTy() { return B.getInt8PtrTy(); }
  Type *getBuilderPtrTy() { return B.getInt8PtrTy(); }
  Type *getTypePtrTy() { return B.getInt8PtrTy(); }
  Type *getValuePtrTy() { return B.getInt8PtrTy(); }

  Constant *getAPIFunction(StringRef Name, Type *Result,
                           ArrayRef<Type *> Params) {
    return B.GetInsertBlock()->getModule()->getOrInsertFunction(
        Name, FunctionType::get(Result, Params, false));
  }

  IRBuilder &B;
  Value *StagedContext;
  Value *StagedBuilder = nullptr;
};

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createModule(const Twine &ModuleId,
                                                      const Twine &InstName) {
  return B.CreateCall(
      getAPIFunction("LLVMModuleCreateWithNameInContext", getModulePtrTy(),
                     {B.getInt8PtrTy(), getContextPtrTy()}),
      {B.CreateGlobalStringPtr(ModuleId.str(), GlobalPrefix), StagedContext},
      InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createFunction(
    FunctionType *Type, GlobalValue::LinkageTypes Linkage, const Twine &Name,
    Value *StagedModule, const Twine &InstName) {
  auto *F = B.CreateCall(
      getAPIFunction("LLVMAddFunction", getFunctionPtrTy(),
                     {getModulePtrTy(), B.getInt8PtrTy(), getTypePtrTy()}),
      {StagedModule, B.CreateGlobalStringPtr(Name.str(), GlobalPrefix),
       createType(Type)},
      InstName);

  if (Linkage != GlobalValue::ExternalLinkage) {
    setLinkage(F, Linkage);
  }

  return F;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createBasicBlock(
    const Twine &Name, Value *StagedFunction, const Twine &InstName) {
  return B.CreateCall(
      getAPIFunction("LLVMAppendBasicBlockInContext", getBasicBlockPtrTy(),
                     {getContextPtrTy(), getFunctionPtrTy(), B.getInt8PtrTy()}),
      {StagedContext, StagedFunction,
       B.CreateGlobalStringPtr(Name.str(), GlobalPrefix)},
      InstName);
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
                                     {getValuePtrTy(), B.getInt32Ty()}),
                      {Global, B.getInt32(CAPILinkage)});
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createBuilder(const Twine &InstName) {
  assert(!StagedBuilder && "Staged IRBuilder is already created");

  auto *SB =
      B.CreateCall(getAPIFunction("LLVMCreateBuilderInContext",
                                  getBuilderPtrTy(), {getContextPtrTy()}),
                   {StagedContext}, InstName);

  StagedBuilder = SB;
  return SB;
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::positionBuilderAtEnd(Value *Block,
                                                 const Twine &InstName) {
  return B.CreateCall(getAPIFunction("LLVMPositionBuilderAtEnd", B.getVoidTy(),
                                     {getBuilderPtrTy(), getBasicBlockPtrTy()}),
                      {StagedBuilder, Block}, InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::disposeBuilder(const Twine &InstName) {
  auto *Inst = B.CreateCall(
      getAPIFunction("LLVMDisposeBuilder", B.getVoidTy(), {getBuilderPtrTy()}),
      {StagedBuilder}, InstName);

  StagedBuilder = nullptr;
  return Inst;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createType(Type *Ty,
                                                    const Twine &InstName) {
  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    return B.CreateCall(getAPIFunction("LLVMVoidTypeInContext", getTypePtrTy(),
                                       {getContextPtrTy()}),
                        {StagedContext}, InstName);

  case Type::IntegerTyID: {
    auto *IT = cast<IntegerType>(Ty);

    return B.CreateCall(getAPIFunction("LLVMIntTypeInContext", getTypePtrTy(),
                                       {getContextPtrTy(), B.getInt32Ty()}),
                        {StagedContext, B.getInt32(IT->getBitWidth())},
                        InstName);
  }

  case Type::FunctionTyID: {
    auto *FT = cast<FunctionType>(Ty);
    auto *Params =
        B.CreateAlloca(getTypePtrTy(), B.getInt32(FT->getNumParams()));

    for (unsigned ParamIndex = 0; ParamIndex < FT->getNumParams();
         ++ParamIndex) {
      B.CreateStore(createType(FT->getParamType(ParamIndex)),
                    B.CreateGEP(Params, B.getInt32(ParamIndex)));
    }

    return B.CreateCall(
        getAPIFunction("LLVMFunctionType", getTypePtrTy(),
                       {getTypePtrTy(), PointerType::getUnqual(getTypePtrTy()),
                        B.getInt32Ty(), B.getInt32Ty()}),
        {createType(FT->getReturnType()), Params,
         B.getInt32(FT->getNumParams()), B.getInt32(false)},
        InstName);
  }

  default:
    llvm_unreachable("Unsupported type");
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createInstruction(Instruction *Inst) {
  assert(isa<ReturnInst>(Inst) && !cast<ReturnInst>(Inst)->getReturnValue() &&
         "Instruction not implemented");

  return B.CreateCall(
      getAPIFunction("LLVMBuildRetVoid", getValuePtrTy(), {getBuilderPtrTy()}),
      {StagedBuilder}, Inst->getName());
}

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
