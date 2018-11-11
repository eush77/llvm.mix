//===- MixContext.h ---------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines class MixContext that represents a collection of dynamic
// values used during generation of the next stage.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_MIXCONTEXT_H
#define LLVM_LIB_TRANSFORMS_MIX_MIXCONTEXT_H

#include "CAPIFunctions.h"
#include "Types.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerSumType.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>
#include <string>
#include <tuple>

namespace llvm {

class LLVMContext;

namespace mix {

enum ValueDescTag {
  VDT_None,
  VDT_Context,
  VDT_Builder,
  VDT_Module,
  VDT_Type,
};

using ValueDesc =
    PointerSumType<ValueDescTag,
                   PointerSumTypeMember<VDT_None, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Context, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Builder, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Module, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Type, Type *>>;

// Get IR type for value descriptor.
Type *getType(LLVMContext &, ValueDesc);

// Get a name that can be used for IR values generated from value descriptor.
std::string getName(ValueDesc);

// A mapping from sequential unsigned indices to descriptors of requested
// values
class MixContextTable {
public:
  // Find descriptor in the context table.
  Optional<unsigned> getExistingIndex(ValueDesc) const;

  // Get or insert a descriptor in the context table and return an index.
  unsigned getIndex(ValueDesc);

  static PointerType *getTablePtrTy(LLVMContext &C) {
    return Type::getInt8PtrTy(C)->getPointerTo();
  }

  // Build code to allocate and populate the context table. Returns dynamic
  // table pointer that can be used to creat MixContext instances.
  template <typename IRBuilder>
  Value *build(IRBuilder &, Value *DynContext, StringRef ModuleID);

private:
  template <typename IRBuilder> Value *buildEntry(IRBuilder &, ValueDesc);

  template <typename IRBuilder> Value *buildContext(IRBuilder &B) {
    return buildEntry(B, ValueDesc::create<VDT_Context>({}));
  }

  template <typename IRBuilder>
  Value *buildType(IRBuilder &, Type *, StringRef Name = "");

  template <typename IRBuilder> static Module &getModule(IRBuilder &B) {
    return *B.GetInsertBlock()->getModule();
  }

  // Descriptor table
  MapVector<ValueDesc, unsigned> Desc;

  // Map of available values
  DenseMap<ValueDesc, Value *> Values;
};

// A pair of static and dynamic context table pointers. This class provides
// dynamic access to context table entries.
class MixContext {
public:
  MixContext(MixContextTable &T, Value *TP) : T(T), TP(TP) {
    assert(TP->getType() == MixContextTable::getTablePtrTy(TP->getContext()) &&
           "Not a context table");
  }

  MixContextTable &getTable() const { return T; }
  Value *getTablePointer() const { return TP; }

  // Build code to release any resources acquired for the context table.
  template <typename IRBuilder> void dispose(IRBuilder &) const;

  // Get staged LLVMContextRef.
  template <typename IRBuilder> Value *getContext(IRBuilder &B) {
    return getValue(B, ValueDesc::create<VDT_Context>({}));
  }

  // Get staged LLVMBuilderRef.
  template <typename IRBuilder> Value *getBuilder(IRBuilder &B) {
    return getValue(B, ValueDesc::create<VDT_Builder>({}));
  }

  // Get staged LLVMModuleRef.
  template <typename IRBuilder> Value *getModule(IRBuilder &B) {
    return getValue(B, ValueDesc::create<VDT_Module>({}));
  }

  // Get staged LLVMTypeRef.
  template <typename IRBuilder> Value *getType(IRBuilder &B, Type *Ty) {
    return getValue(B, ValueDesc::create<VDT_Type>(Ty));
  }

private:
  // Resolve value descriptor in the context table.
  template <typename IRBuilder> Value *getValue(IRBuilder &B, ValueDesc VD) {
    return getValue(B, VD, T.getIndex(VD));
  }

  // Same as `getValue', but return null if descriptor is not in the table.
  template <typename IRBuilder>
  Value *getExistingValue(IRBuilder &B, ValueDesc VD) const;

  template <typename IRBuilder>
  Value *getValue(IRBuilder &B, ValueDesc VD, unsigned Index) const {
    return B.CreateBitCast(
        B.CreateLoad(B.CreateGEP(TP, B.getInt32(Index)), getName(VD)),
        mix::getType(B.getContext(), VD), getName(VD));
  }

  MixContextTable &T;
  Value *TP;
};

template <typename IRBuilder>
Value *MixContextTable::build(IRBuilder &B, Value *DynContext,
                              StringRef ModuleID) {
  auto ContextVD = ValueDesc::create<VDT_Context>({});
  auto ModuleVD = ValueDesc::create<VDT_Module>({});

  // Build context.
  Values[ContextVD] = DynContext = B.CreateBitCast(
      DynContext, getContextPtrTy(B.getContext()), getName(ContextVD));

  // Build module.
  Values[ModuleVD] =
      B.CreateCall(getModuleCreateWithNameInContextFn(getModule(B)),
                   {B.CreateGlobalStringPtr(ModuleID, "moduleid"), DynContext},
                   getName(ModuleVD));

  // Allocate the table.
  Value *Table = B.CreateAlloca(getTablePtrTy(B.getContext())->getElementType(),
                                B.getInt32(Desc.size()), "mix.context");

  for (auto &P : Desc) {
    ValueDesc VD;
    unsigned Index;
    std::tie(VD, Index) = P;

    B.CreateStore(
        B.CreateBitCast(buildEntry(B, VD),
                        getTablePtrTy(B.getContext())->getElementType(),
                        getName(VD)),
        B.CreateGEP(Table, B.getInt32(Index)));
  }

  Values.clear();
  return Table;
}

template <typename IRBuilder>
Value *MixContextTable::buildEntry(IRBuilder &B, ValueDesc VD) {
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
    V = B.CreateCall(getCreateBuilderInContextFn(getModule(B)), buildContext(B),
                     getName(VD));
    break;

  case VDT_Type:
    V = buildType(B, VD.get<VDT_Type>(), getName(VD));
    break;
  }

  assert(V && "Unsupported value descriptor");
  return V;
}

template <typename IRBuilder>
Value *MixContextTable::buildType(IRBuilder &B, Type *Ty, StringRef Name) {
  Value *&V = Values[ValueDesc::create<VDT_Type>(Ty)];

  if (V)
    return V;

  switch (Ty->getTypeID()) {
  case Type::VoidTyID:
    V = B.CreateCall(getVoidTypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::HalfTyID:
    V = B.CreateCall(getHalfTypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::FloatTyID:
    V = B.CreateCall(getFloatTypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::DoubleTyID:
    V = B.CreateCall(getDoubleTypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::X86_FP80TyID:
    V = B.CreateCall(getX86FP80TypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::FP128TyID:
    V = B.CreateCall(getFP128TypeInContextFn(getModule(B)), buildContext(B),
                     Name);
    break;

  case Type::PPC_FP128TyID:
    V = B.CreateCall(getPPCFP128TypeInContextFn(getModule(B)), buildContext(B),
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
      V = B.CreateCall(
          getModule(B).getOrInsertFunction(
              "LLVMInt" + std::to_string(BitWidth) + "TypeInContext",
              FunctionType::get(getTypePtrTy(B.getContext()),
                                {getContextPtrTy(B.getContext())}, false)),
          buildContext(B), Name);
      break;

    default:
      V = B.CreateCall(
          getIntTypeInContextFn(getModule(B)),
          {buildContext(B),
           ConstantInt::get(getUnsignedIntTy(B.getContext()), BitWidth)},
          Name);
    }
    break;
  }

  case Type::FunctionTyID: {
    auto *FT = cast<FunctionType>(Ty);
    Value *Params;

    if (FT->getNumParams()) {
      Params = B.CreateAlloca(getTypePtrTy(B.getContext()),
                              B.getInt32(FT->getNumParams()));

      for (unsigned ParamIndex = 0; ParamIndex < FT->getNumParams();
           ++ParamIndex) {
        B.CreateStore(buildType(B, FT->getParamType(ParamIndex)),
                      B.CreateGEP(Params, B.getInt32(ParamIndex)));
      }
    } else {
      Params = ConstantPointerNull::get(
          PointerType::getUnqual(getTypePtrTy(B.getContext())));
    }

    V = B.CreateCall(
        getFunctionTypeFn(getModule(B)),
        {buildType(B, FT->getReturnType()), Params,
         ConstantInt::get(getUnsignedIntTy(B.getContext()), FT->getNumParams()),
         ConstantInt::get(getBoolTy(B.getContext()), FT->isVarArg())},
        Name);
    break;
  }

  case Type::PointerTyID: {
    auto *PT = cast<PointerType>(Ty);

    V = B.CreateCall(getPointerTypeFn(getModule(B)),
                     {buildType(B, PT->getElementType()),
                      ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                       PT->getAddressSpace())},
                     Name);
    break;
  }

  case Type::StructTyID: {
    auto *ST = cast<StructType>(Ty);
    Value *Elements;

    if (ST->getNumElements()) {
      Elements = B.CreateAlloca(getTypePtrTy(B.getContext()),
                                B.getInt32(ST->getNumElements()));

      for (unsigned ElNum = 0; ElNum < ST->getNumElements(); ++ElNum) {
        B.CreateStore(buildType(B, ST->getElementType(ElNum)),
                      B.CreateGEP(Elements, B.getInt32(ElNum)));
      }
    } else {
      Elements = ConstantPointerNull::get(
          PointerType::getUnqual(getTypePtrTy(B.getContext())));
    }

    if (ST->hasName()) {
      V = B.CreateCall(
          getStructCreateNamedFn(getModule(B)),
          {buildContext(B),
           B.CreateGlobalStringPtr(ST->getName(), ST->getName() + ".name")},
          Name);

      if (!ST->isOpaque()) {
        B.CreateCall(
            getStructSetBodyFn(getModule(B)),
            {V, Elements,
             ConstantInt::get(getUnsignedIntTy(B.getContext()),
                              ST->getNumElements()),
             ConstantInt::get(getBoolTy(B.getContext()), ST->isPacked())});
      }
    } else {
      V = B.CreateCall(
          getStructTypeInContextFn(getModule(B)),
          {buildContext(B), Elements,
           ConstantInt::get(getUnsignedIntTy(B.getContext()),
                            ST->getNumElements()),
           ConstantInt::get(getBoolTy(B.getContext()), ST->isPacked())},
          Name);
    }
    break;
  }
  }

  assert(V && "Unsupported type");
  return V;
}

template <typename IRBuilder>
Value *MixContext::getExistingValue(IRBuilder &B, ValueDesc VD) const {
  if (auto Index = T.getExistingIndex(VD))
    return getValue(B, VD, *Index);
  else
    return nullptr;
}

template <typename IRBuilder>
void MixContext::dispose(IRBuilder &B) const {
  if (auto DB = getExistingValue(B, ValueDesc::create<VDT_Builder>({})))
    B.CreateCall(getDisposeBuilderFn(*B.GetInsertBlock()->getModule()), DB);
}

} // namespace mix

template <> struct DenseMapInfo<mix::ValueDesc> {
  static mix::ValueDesc getEmptyKey() {
    return mix::ValueDesc::create<mix::VDT_None>('\0');
  }

  static mix::ValueDesc getTombstoneKey() {
    return mix::ValueDesc::create<mix::VDT_None>('\xFF');
  }

  static unsigned getHashValue(mix::ValueDesc CGD) {
    return static_cast<unsigned>(CGD.getOpaqueValue());
  }

  static bool isEqual(mix::ValueDesc Left, mix::ValueDesc Right) {
    return Left == Right;
  }
};

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_MIXCONTEXT_H
