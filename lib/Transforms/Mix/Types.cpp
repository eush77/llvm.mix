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
// The names don't have to match LLVM-C names, but try to stick to LLVM-C
// names for convenience and to limit bitcasting.
//
//===----------------------------------------------------------------------===//

#include "Types.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

using namespace llvm;

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
