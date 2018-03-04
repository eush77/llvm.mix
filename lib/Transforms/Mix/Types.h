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

class LLVMContext;
class Type;

namespace mix {

// C types.
Type *getCharPtrTy(LLVMContext &);
Type *getUnsignedIntTy(LLVMContext &);
Type *getUnsignedLongLongIntTy(LLVMContext &);

// LLVM-C types.
Type *getBoolTy(LLVMContext &);
Type *getIntPredicateTy(LLVMContext &);
Type *getLinkageTy(LLVMContext &);
Type *getOpcodeTy(LLVMContext &);

// IR types.
Type *getBasicBlockPtrTy(LLVMContext &);
Type *getBuilderPtrTy(LLVMContext &);
Type *getContextPtrTy(LLVMContext &);
Type *getModulePtrTy(LLVMContext &);
Type *getTypePtrTy(LLVMContext &);
Type *getValuePtrTy(LLVMContext &);

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_TYPES_H
