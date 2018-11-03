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
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>
#include <string>
#include <tuple>

namespace llvm {

class LLVMContext;
class PointerType;

namespace mix {

enum ValueDescTag {
  VDT_None,
  VDT_Context,
  VDT_Builder,
  VDT_Module,
};

using ValueDesc =
    PointerSumType<ValueDescTag,
                   PointerSumTypeMember<VDT_None, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Context, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Builder, PointerEmbeddedInt<char>>,
                   PointerSumTypeMember<VDT_Module, PointerEmbeddedInt<char>>>;

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

  template <ValueDescTag VDT, typename IRBuilder>
  Value *buildEntry(IRBuilder &B) {
    return buildEntry(B, ValueDesc::create<VDT>({}));
  }

  template <ValueDescTag VDT, typename IRBuilder, typename ValueDescPointer>
  Value *buildEntry(IRBuilder &B, ValueDescPointer VDP) {
    return buildEntry(B, ValueDesc::create<VDT>(VDP));
  }

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
        getType(B.getContext(), VD), getName(VD));
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
    V = B.CreateCall(getCreateBuilderInContextFn(getModule(B)),
                     buildEntry<VDT_Context>(B), getName(VD));
    break;
  }

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
