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

PointerType *mix::getCharPtrTy(LLVMContext &Context) {
  return PointerType::getUnqual(getIntegerType<char>(Context));
}

Type *mix::getDoubleTy(LLVMContext &Context) {
  switch (sizeof(double) * CHAR_BIT) {
  case 16:
    return Type::getHalfTy(Context);

  case 32:
    return Type::getFloatTy(Context);

  case 64:
    return Type::getDoubleTy(Context);

  case 128:
    return Type::getFP128Ty(Context);
  }

  llvm_unreachable("Unsupported double type width");
}

IntegerType *mix::getUnsignedIntTy(LLVMContext &Context) {
  return getIntegerType<unsigned int>(Context);
}

IntegerType *mix::getUnsignedLongLongIntTy(LLVMContext &Context) {
  return getIntegerType<unsigned long long>(Context);
}

IntegerType *mix::getBoolTy(LLVMContext &Context) {
  return getIntegerType<LLVMBool>(Context);
}

IntegerType *mix::getIntPredicateTy(LLVMContext &Context) {
  return getIntegerType<LLVMIntPredicate>(Context);
}

IntegerType *mix::getLinkageTy(LLVMContext &Context) {
  return getIntegerType<LLVMLinkage>(Context);
}

IntegerType *mix::getOpcodeTy(LLVMContext &Context) {
  return getIntegerType<LLVMOpcode>(Context);
}

namespace {

// Get opaque struct type by name from the context. If no such type exists, or
// if it's not an opaque struct type, create a new type.
StructType *getOpaqueType(LLVMContext &Context, StringRef Name) {
  if (auto *Ty = StructType::getTypeByName(Context, Name)) {
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

PointerType *mix::getBasicBlockPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueBasicBlock");
}

PointerType *mix::getBuilderPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueBuilder");
}

PointerType *mix::getContextPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueContext");
}

PointerType *mix::getMetadataPtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueMetadata");
}

PointerType *mix::getModulePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueModule");
}

PointerType *mix::getTypePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueType");
}

PointerType *mix::getValuePtrTy(LLVMContext &Context) {
  return getOpaquePointerType(Context, "struct.LLVMOpaqueValue");
}
