//===- Types.h - IR types for use in generated code -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares singleton struct type constructors to be used in
// generated code when referring to IR objects such as contexts, modules, etc,
// as well as other types used in the C API.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_TYPES_H
#define LLVM_LIB_TRANSFORMS_MIX_TYPES_H

namespace llvm {

class IntegerType;
class LLVMContext;
class PointerType;
class Type;

namespace mix {

// C types.
PointerType *getCharPtrTy(LLVMContext &);
Type *getDoubleTy(LLVMContext &);
IntegerType *getUnsignedIntTy(LLVMContext &);
IntegerType *getUnsignedLongLongIntTy(LLVMContext &);

// LLVM-C types.
IntegerType *getBoolTy(LLVMContext &);
IntegerType *getIntPredicateTy(LLVMContext &);
IntegerType *getLinkageTy(LLVMContext &);
IntegerType *getOpcodeTy(LLVMContext &);

// IR types.
PointerType *getBasicBlockPtrTy(LLVMContext &);
PointerType *getBuilderPtrTy(LLVMContext &);
PointerType *getContextPtrTy(LLVMContext &);
PointerType *getMetadataPtrTy(LLVMContext &);
PointerType *getModulePtrTy(LLVMContext &);
PointerType *getTypePtrTy(LLVMContext &);
PointerType *getValuePtrTy(LLVMContext &);

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_TYPES_H
