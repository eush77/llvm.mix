//===----- CGMix.cpp - Emit LLVM Code for Mix specializers ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code generator for Mix specializers.
//
//===----------------------------------------------------------------------===//

#include "CGCall.h"
#include "CodeGenFunction.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include <algorithm>
#include <iterator>
#include <utility>

using namespace clang;
using namespace CodeGen;

void CodeGenFunction::EmitMixSpecializerBody(FunctionArgList &Args) {
  FunctionDecl *F = nullptr;

  if (auto *MA = CurFuncDecl->getAttr<MixAttr>()) {
    F = MA->getMixedFunction();
  }

  auto *FID = llvm::ConstantExpr::getBitCast(
      CGM.GetAddrOfFunction(F), llvm::Type::getInt8PtrTy(getLLVMContext()));

  llvm::SmallVector<llvm::Value *, 8> ArgValues{FID};

  std::transform(Args.begin(), Args.end(), std::back_inserter(ArgValues),
                 [&](const VarDecl *A) {
                   return Builder.CreateLoad(GetAddrOfLocalVar(A),
                                             A->getName());
                 });

  ArgValues[1] = Builder.CreateBitCast(
      ArgValues[1], llvm::Type::getInt8PtrTy(getLLVMContext()),
      ArgValues[1]->getName());

  llvm::Value *RetVal = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::mix), ArgValues, "function");
  RetVal = Builder.CreateBitCast(RetVal, CurFn->getReturnType(), "function");

  Builder.CreateStore(RetVal, ReturnValue);
}
