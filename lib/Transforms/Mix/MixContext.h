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
#include <string>
#include <vector>

namespace llvm {

class BasicBlock;
class Function;
class Instruction;
class LLVMContext;
class Module;
class PointerType;

namespace mix {

// Common base for classes working with value descriptors
class MixContextBase {
public:
  enum ValueDescTag {
    VDT_None,
    VDT_Context,
    VDT_Builder,
    VDT_Module,
    VDT_Function,
    VDT_MDKindID,
    VDT_Type,
  };

  using ValueDesc = PointerSumType<
      ValueDescTag, PointerSumTypeMember<VDT_None, PointerEmbeddedInt<char>>,
      PointerSumTypeMember<VDT_Context, PointerEmbeddedInt<char>>,
      PointerSumTypeMember<VDT_Builder, PointerEmbeddedInt<char>>,
      PointerSumTypeMember<VDT_Module, PointerEmbeddedInt<char>>,
      PointerSumTypeMember<VDT_Function, Function *>,
      PointerSumTypeMember<VDT_MDKindID, PointerEmbeddedInt<unsigned>>,
      PointerSumTypeMember<VDT_Type, Type *>>;

  explicit MixContextBase(LLVMContext &C);

  // Get IR type for value descriptor
  Type *getType(ValueDesc VD) const;

  // Get a name that can be used for IR values generated from value descriptor
  std::string getName(ValueDesc VD) const;

  // Get metadata kind name from kind ID
  StringRef getMDKindName(unsigned KindID) const { return MDKindNames[KindID]; }

private:
  LLVMContext &C;
  std::vector<StringRef> MDKindNames;
};

// A mapping from sequential unsigned indices to descriptors of requested
// values
class MixContextTable : public MixContextBase {
public:
  explicit MixContextTable(LLVMContext &C) : MixContextBase(C) {}

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
  Value *buildModule() { return buildEntry(ValueDesc::create<VDT_Module>({})); }
  Value *buildType(Type *);
  Value *buildContext() {
    return buildEntry(ValueDesc::create<VDT_Context>({}));
  }

  IRBuilder<> *B = nullptr;

  // Descriptor table
  MapVector<ValueDesc, unsigned> Desc;

  // Map of available values
  DenseMap<ValueDesc, Value *> Values;
};

// A pair of static and dynamic context table pointers. This class provides
// dynamic access to context table entries.
class MixContext : public MixContextBase {
public:
  MixContext(MixContextTable &T, Value *TP, Instruction *InsertBefore)
      : MixContextBase(TP->getContext()), T(T), TP(TP), B(InsertBefore) {
    assert(TP->getType() == MixContextTable::getTablePtrTy(TP->getContext()) &&
           "Not a context table");
  }

  MixContext(MixContextTable &T, Value *TP, BasicBlock *InsertAtEnd)
      : MixContextBase(TP->getContext()), T(T), TP(TP), B(InsertAtEnd) {
    assert(TP->getType() == MixContextTable::getTablePtrTy(TP->getContext()) &&
           "Not a context table");
  }

  MixContextTable &getTable() const { return T; }
  Value *getTablePointer() const { return TP; }

  // Build code to release any resources acquired for the context table.
  void dispose();

  // Get staged LLVMContextRef.
  Instruction *getContext() {
    return getValue(ValueDesc::create<VDT_Context>({}));
  }

  // Get staged LLVMBuilderRef.
  Instruction *getBuilder() {
    return getValue(ValueDesc::create<VDT_Builder>({}));
  }

  // Get staged LLVMModuleRef.
  Instruction *getModule() {
    return getValue(ValueDesc::create<VDT_Module>({}));
  }

  // Get staged declared function.
  Instruction *getFunction(Function *F) {
    return getValue(ValueDesc::create<VDT_Function>(F));
  }

  // Get metadata kind ID in the staged context.
  Instruction *getMDKindID(unsigned KindID) {
    return getValue(ValueDesc::create<VDT_MDKindID>(KindID));
  }

  // Get staged LLVMTypeRef.
  Instruction *getType(Type *Ty) {
    return getValue(ValueDesc::create<VDT_Type>(Ty));
  }

private:
  using MixContextBase::getType;

  // Resolve value descriptor in the context table.
  Instruction *getValue(ValueDesc VD);
  Instruction *getValue(ValueDesc VD, unsigned Index);

  // Same as `getValue', but return null if descriptor is not in the table.
  Instruction *getExistingValue(ValueDesc VD);

  MixContextTable &T;
  Value *TP;
  IRBuilder<> B;

  // Map of available values
  DenseMap<ValueDesc, Instruction *> Values;
};

} // namespace mix

template <> struct DenseMapInfo<mix::MixContextBase::ValueDesc> {
  static mix::MixContextBase::ValueDesc getEmptyKey() {
    return mix::MixContextBase::ValueDesc::create<
        mix::MixContextBase::VDT_None>('\0');
  }

  static mix::MixContextBase::ValueDesc getTombstoneKey() {
    return mix::MixContextBase::ValueDesc::create<
        mix::MixContextBase::VDT_None>('\xFF');
  }

  static unsigned getHashValue(mix::MixContextBase::ValueDesc CGD) {
    return static_cast<unsigned>(CGD.getOpaqueValue());
  }

  static bool isEqual(mix::MixContextBase::ValueDesc Left,
                      mix::MixContextBase::ValueDesc Right) {
    return Left == Right;
  }
};

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_MIXCONTEXT_H
