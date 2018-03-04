//===- Types.cpp ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements IR type constructors.
//
// The names of opaque struct types don't have to match LLVM-C names, but try
// to stick to LLVM-C names for convenience and to limit bitcasting.
//
//===----------------------------------------------------------------------===//

#include "Types.h"

#include "llvm-c/Core.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include <cassert>
#include <climits>
#include <type_traits>

using namespace llvm;

namespace {

// Get IR integer type from C++ integral or enumeration type.
template <typename T> IntegerType *getIntegerType(LLVMContext &Context) {
  static_assert(std::is_integral<T>::value || std::is_enum<T>::value,
                "Not an integral or enumeration type");
  return IntegerType::get(Context, sizeof(T) * CHAR_BIT);
}

} // namespace

Type *mix::getCharPtrTy(LLVMContext &Context) {
  return PointerType::getUnqual(getIntegerType<char>(Context));
}

Type *mix::getUnsignedIntTy(LLVMContext &Context) {
  return getIntegerType<unsigned int>(Context);
}

Type *mix::getUnsignedLongLongIntTy(LLVMContext &Context) {
  return getIntegerType<unsigned long long>(Context);
}

Type *mix::getBoolTy(LLVMContext &Context) {
  return getIntegerType<LLVMBool>(Context);
}

Type *mix::getIntPredicateTy(LLVMContext &Context) {
  return getIntegerType<LLVMIntPredicate>(Context);
}

Type *mix::getLinkageTy(LLVMContext &Context) {
  return getIntegerType<LLVMLinkage>(Context);
}

Type *mix::getOpcodeTy(LLVMContext &Context) {
  return getIntegerType<LLVMOpcode>(Context);
}

namespace {

// Get opaque struct type by name from the context. If no such type exists, or
// if it's not an opaque struct type, create a new type.
StructType *getOpaqueType(LLVMContext &Context, StringRef Name) {
  Module M("", Context);

  if (auto *Ty = M.getTypeByName(Name)) {
    if (Ty->isOpaque()) {
      return Ty;
    }
  }

  return StructType::create(Context, Name);
}

// Get a pointer type to a named opaque struct.
PointerType *getOpaquePointerType(LLVMContext &Context, StringRef Name) {
  return PointerType::getUnqual(getOpaqueType(Context, Name));
}

} // namespace

Type *mix::getBasicBlockPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueBasicBlock");
}

Type *mix::getBuilderPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueBuilder");
}

Type *mix::getContextPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueContext");
}

Type *mix::getModulePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueModule");
}

Type *mix::getTypePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueType");
}

Type *mix::getValuePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueValue");
}
