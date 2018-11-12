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

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerSumType.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include <cassert>

namespace llvm {

class BasicBlock;
class Instruction;
class LLVMContext;
class Module;
class PointerType;

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
  Value *build(Value *DynContext, StringRef ModuleID,
               Instruction *InsertBefore);
  Value *build(Value *DynContext, StringRef ModuleID, BasicBlock *InsertAtEnd);

private:
  Module &getModule() const { return *B->GetInsertBlock()->getModule(); }

  Value *build(Value *DynContext, StringRef ModuleID);
  Value *buildEntry(ValueDesc);
  Value *buildType(Type *, StringRef Name = "");
  Value *buildContext() {
    return buildEntry(ValueDesc::create<VDT_Context>({}));
  }

  IRBuilder<> *B;

  // Descriptor table
  MapVector<ValueDesc, unsigned> Desc;

  // Map of available values
  DenseMap<ValueDesc, Value *> Values;
};

// A pair of static and dynamic context table pointers. This class provides
// dynamic access to context table entries.
class MixContext {
public:
  MixContext(MixContextTable &T, Value *TP, Instruction *InsertBefore)
      : T(T), TP(TP), B(InsertBefore) {
    assert(TP->getType() == MixContextTable::getTablePtrTy(TP->getContext()) &&
           "Not a context table");
  }

  MixContext(MixContextTable &T, Value *TP, BasicBlock *InsertAtEnd)
      : T(T), TP(TP), B(InsertAtEnd) {
    assert(TP->getType() == MixContextTable::getTablePtrTy(TP->getContext()) &&
           "Not a context table");
  }

  MixContextTable &getTable() const { return T; }
  Value *getTablePointer() const { return TP; }

  // Build code to release any resources acquired for the context table.
  void dispose();

  // Get staged LLVMContextRef.
  Value *getContext() { return getValue(ValueDesc::create<VDT_Context>({})); }

  // Get staged LLVMBuilderRef.
  Value *getBuilder() { return getValue(ValueDesc::create<VDT_Builder>({})); }

  // Get staged LLVMModuleRef.
  Value *getModule() { return getValue(ValueDesc::create<VDT_Module>({})); }

  // Get staged LLVMTypeRef.
  Value *getType(Type *Ty) { return getValue(ValueDesc::create<VDT_Type>(Ty)); }

private:
  // Resolve value descriptor in the context table.
  Value *getValue(ValueDesc VD) { return getValue(VD, T.getIndex(VD)); }
  Value *getValue(ValueDesc VD, unsigned Index);

  // Same as `getValue', but return null if descriptor is not in the table.
  Value *getExistingValue(ValueDesc VD);

  MixContextTable &T;
  Value *TP;
  IRBuilder<> B;
};

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
