//===- MixContext.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements class MixContext.
//
//===----------------------------------------------------------------------===//

#include "MixContext.h"
#include "CAPIFunctions.h"
#include "Types.h"

#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SaveAndRestore.h"

#include <algorithm>
#include <cassert>
#include <iterator>
#include <string>
#include <tuple>

using namespace llvm;
using namespace mix;

MixContextBase::MixContextBase(LLVMContext &C) : C(C) {
  SmallVector<StringRef, 32> MDKindNames;

  C.getMDKindNames(MDKindNames);
  std::copy(MDKindNames.begin(), MDKindNames.end(),
            std::back_inserter(this->MDKindNames));
}

Type *MixContextBase::getType(ValueDesc VD) const {
  switch (VD.getTag()) {
  case VDT_None:
    llvm_unreachable("VDT_None ValueDesc has no IR type");

  case VDT_Context:
    return getContextPtrTy(C);

  case VDT_Builder:
    return getBuilderPtrTy(C);

  case VDT_Module:
    return getModulePtrTy(C);

  case VDT_Function:
    return getValuePtrTy(C);

  case VDT_MDKindID:
    return getUnsignedIntTy(C);

  case VDT_Type:
    return getTypePtrTy(C);
  }

  llvm_unreachable("Unhandled ValueDesc");
}

std::string MixContextBase::getName(ValueDesc VD) const {
  switch (VD.getTag()) {
  case VDT_None:
    llvm_unreachable("Generating name for VDT_None ValueDesc");

  case VDT_Context:
    return "context";

  case VDT_Builder:
    return "builder";

  case VDT_Module:
    return "module";

  case VDT_Function:
    return "function." + VD.get<VDT_Function>()->getName().str();

  case VDT_MDKindID:
    return "metadata." + MDKindNames[VD.get<VDT_MDKindID>()].str();

  case VDT_Type:
    return "type." + VD.get<VDT_Type>()->getMangledTypeStr();
  }

  llvm_unreachable("Unhandled ValueDesc");
}

Optional<unsigned> MixContextTable::getExistingIndex(ValueDesc VD) const {
  auto Iter = Desc.find(VD);

  if (Iter == Desc.end())
    return None;

  return Iter->second;
}

unsigned MixContextTable::getIndex(ValueDesc VD) {
  if (auto Index = getExistingIndex(VD))
    return *Index;

  unsigned Index = Desc.size();
  return Desc[VD] = Index;
}

Value *MixContextTable::build(Value *DynContext, StringRef ModuleID,
                              Instruction *InsertBefore) {
  IRBuilder<> B(InsertBefore);
  SaveAndRestore<IRBuilder<> *> RestoreBOnExit(this->B, &B);
  return build(DynContext, ModuleID);
}

Value *MixContextTable::build(Value *DynContext, StringRef ModuleID,
                              BasicBlock *InsertAtEnd) {
  IRBuilder<> B(InsertAtEnd);
  SaveAndRestore<IRBuilder<> *> RestoreBOnExit(this->B, &B);
  return build(DynContext, ModuleID);
}

Value *MixContextTable::build(Value *DynContext, StringRef ModuleID) {
  auto ContextVD = ValueDesc::create<VDT_Context>({});
  auto ModuleVD = ValueDesc::create<VDT_Module>({});

  // Build context.
  Values[ContextVD] = DynContext = B->CreateBitCast(
      DynContext, getContextPtrTy(B->getContext()), getName(ContextVD));

  // Build module.
  Values[ModuleVD] = B->CreateCall(
      getModuleCreateWithNameInContextFn(getModule()),
      {B->CreateGlobalStringPtr(ModuleID, "moduleid"), DynContext},
      getName(ModuleVD));

  // Allocate the table.
  Value *Table =
      B->CreateAlloca(getTablePtrTy(B->getContext())->getElementType(),
                      B->getInt32(Desc.size()), "mix.context");

  for (auto &P : Desc) {
    ValueDesc VD;
    unsigned Index;
    std::tie(VD, Index) = P;

    B->CreateStore(B->CreateBitOrPointerCast(
                       buildEntry(VD),
                       getTablePtrTy(B->getContext())->getElementType(),
                       getName(VD)),
                   B->CreateGEP(Table, B->getInt32(Index)));
  }

  Values.clear();
  return Table;
}

Value *MixContextTable::buildEntry(ValueDesc VD) {
  Value *&V = Values[VD];

  if (V)
    return V;

  switch (VD.getTag()) {
  case VDT_None:
    break;

  case VDT_Context:
  case VDT_Module:
    llvm_unreachable(
        "Descriptor must have been built in MixContextTable::build");
    break;

  case VDT_Builder:
    V = B->CreateCall(getCreateBuilderInContextFn(getModule()), buildContext(),
                      getName(VD));
    break;

  case VDT_Function: {
    Function *F = VD.get<VDT_Function>();

    V = B->CreateCall(
        getAddFunctionFn(getModule()),
        {buildModule(),
         B->CreateGlobalStringPtr(F->getName(), F->getName() + ".name"),
         buildType(F->getFunctionType())},
        getName(VD));
    break;
  }

  case VDT_MDKindID: {
    StringRef Name = getMDKindName(VD.get<VDT_MDKindID>());

    V = B->CreateCall(
        getGetMDKindIDInContextFn(getModule()),
        {buildContext(), B->CreateGlobalStringPtr(Name, Name + ".name"),
         ConstantInt::get(getUnsignedIntTy(B->getContext()), Name.size())});
    break;
  }

  case VDT_Type:
    V = buildType(VD.get<VDT_Type>());
    break;
  }

  assert(V && "Unsupported value descriptor");
  return V;
}

Value *MixContextTable::buildType(Type *Ty) {
  auto VD = ValueDesc::create<VDT_Type>(Ty);
  Value *&V = Values[VD];

  if (V)
    return V;

  std::string Name = getName(VD);

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    V = B->CreateCall(getVoidTypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::HalfTyID:
    V = B->CreateCall(getHalfTypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::FloatTyID:
    V = B->CreateCall(getFloatTypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::DoubleTyID:
    V = B->CreateCall(getDoubleTypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::X86_FP80TyID:
    V = B->CreateCall(getX86FP80TypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::FP128TyID:
    V = B->CreateCall(getFP128TypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::PPC_FP128TyID:
    V = B->CreateCall(getPPCFP128TypeInContextFn(getModule()), buildContext(),
                      Name);
    break;

  case Type::MetadataTyID:
    V = B->CreateCall(getMetadataTypeInContextFn(getModule()), buildContext(),
                      Name);
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
      V = B->CreateCall(
          getModule().getOrInsertFunction(
              "LLVMInt" + std::to_string(BitWidth) + "TypeInContext",
              FunctionType::get(getTypePtrTy(B->getContext()),
                                {getContextPtrTy(B->getContext())}, false)),
          buildContext(), Name);
      break;

    default:
      V = B->CreateCall(
          getIntTypeInContextFn(getModule()),
          {buildContext(),
           ConstantInt::get(getUnsignedIntTy(B->getContext()), BitWidth)},
          Name);
    }
    break;
  }

  case Type::FunctionTyID: {
    auto *FT = cast<FunctionType>(Ty);
    Value *Params;

    if (FT->getNumParams()) {
      Params = B->CreateAlloca(getTypePtrTy(B->getContext()),
                               B->getInt32(FT->getNumParams()));

      for (unsigned ParamIndex = 0; ParamIndex < FT->getNumParams();
           ++ParamIndex) {
        B->CreateStore(buildType(FT->getParamType(ParamIndex)),
                       B->CreateGEP(Params, B->getInt32(ParamIndex)));
      }
    } else {
      Params = ConstantPointerNull::get(
          PointerType::getUnqual(getTypePtrTy(B->getContext())));
    }

    V = B->CreateCall(
        getFunctionTypeFn(getModule()),
        {buildType(FT->getReturnType()), Params,
         ConstantInt::get(getUnsignedIntTy(B->getContext()),
                          FT->getNumParams()),
         ConstantInt::get(getBoolTy(B->getContext()), FT->isVarArg())},
        Name);
    break;
  }

  case Type::StructTyID: {
    auto *ST = cast<StructType>(Ty);

    auto buildElements = [&]() -> Value * {
      if (!ST->getNumElements())
        return ConstantPointerNull::get(
            PointerType::getUnqual(getTypePtrTy(B->getContext())));

      auto *Elements = B->CreateAlloca(getTypePtrTy(B->getContext()),
                                       B->getInt32(ST->getNumElements()));

      for (unsigned ElNum = 0; ElNum < ST->getNumElements(); ++ElNum) {
        B->CreateStore(buildType(ST->getElementType(ElNum)),
                       B->CreateGEP(Elements, B->getInt32(ElNum)));
      }

      return Elements;
    };

    if (ST->hasName()) {
      V = B->CreateCall(
          getStructCreateNamedFn(getModule()),
          {buildContext(),
           B->CreateGlobalStringPtr(ST->getName(), ST->getName() + ".name")},
          Name);

      if (!ST->isOpaque()) {
        B->CreateCall(
            getStructSetBodyFn(getModule()),
            {V, buildElements(),
             ConstantInt::get(getUnsignedIntTy(B->getContext()),
                              ST->getNumElements()),
             ConstantInt::get(getBoolTy(B->getContext()), ST->isPacked())});
      }
    } else {
      V = B->CreateCall(
          getStructTypeInContextFn(getModule()),
          {buildContext(), buildElements(),
           ConstantInt::get(getUnsignedIntTy(B->getContext()),
                            ST->getNumElements()),
           ConstantInt::get(getBoolTy(B->getContext()), ST->isPacked())},
          Name);
    }
    break;
  }

  case Type::ArrayTyID: {
    auto *AT = cast<ArrayType>(Ty);

    V = B->CreateCall(getArrayTypeFn(getModule()),
                      {buildType(AT->getElementType()),
                       ConstantInt::get(getUnsignedIntTy(B->getContext()),
                                        AT->getNumElements())});
    break;
  }

  case Type::PointerTyID: {
    auto *PT = cast<PointerType>(Ty);

    V = B->CreateCall(getPointerTypeFn(getModule()),
                      {buildType(PT->getElementType()),
                       ConstantInt::get(getUnsignedIntTy(B->getContext()),
                                        PT->getAddressSpace())},
                      Name);
    break;
  }
  }

  assert(V && "Unsupported type");
  return V;
}

void MixContext::dispose() {
  if (auto DB = getExistingValue(ValueDesc::create<VDT_Builder>({})))
    B.CreateCall(getDisposeBuilderFn(*B.GetInsertBlock()->getModule()), DB);
}

Instruction *MixContext::getValue(ValueDesc VD) {
  Instruction *&V = Values[VD];
  return V ? V : V = getValue(VD, T.getIndex(VD));
}

Instruction *MixContext::getValue(ValueDesc VD, unsigned Index) {
  return cast<Instruction>(B.CreateBitOrPointerCast(
      B.CreateLoad(B.CreateGEP(TP, B.getInt32(Index)), getName(VD)),
      getType(VD), getName(VD)));
}

Instruction *MixContext::getExistingValue(ValueDesc VD) {
  if (auto Index = T.getExistingIndex(VD))
    return getValue(VD, *Index);
  else
    return nullptr;
}
