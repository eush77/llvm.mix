//===- MixContext.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements class MixContext.
//
//===----------------------------------------------------------------------===//

#include "MixContext.h"
#include "Types.h"

#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/ErrorHandling.h"

#include <string>

using namespace llvm;
using namespace mix;

Type *mix::getType(LLVMContext &C, ValueDesc VD) {
  switch (VD.getTag()) {
  case VDT_None:
    llvm_unreachable("VDT_None ValueDesc has no IR type");

  case VDT_Context:
    return getContextPtrTy(C);

  case VDT_Builder:
    return getBuilderPtrTy(C);

  case VDT_Module:
    return getModulePtrTy(C);

  case VDT_Type:
    return getTypePtrTy(C);
  }

  llvm_unreachable("Unhandled ValueDesc");
}

std::string mix::getName(ValueDesc VD) {
  switch (VD.getTag()) {
  case VDT_None:
    llvm_unreachable("Generating name for VDT_None ValueDesc");

  case VDT_Context:
    return "context";

  case VDT_Builder:
    return "builder";

  case VDT_Module:
    return "module";

  case VDT_Type:
    return "type." + VD.get<VDT_Type>()->getMangledTypeStr();
  }

  llvm_unreachable("Unhandled ValueDesc");
}

Optional<unsigned> MixContextTable::getExistingIndex(ValueDesc VD) const {
  auto Iter = Desc.find(VD);

  if (Iter == Desc.end())
    return None;

  return Iter->second;
}

unsigned MixContextTable::getIndex(ValueDesc VD) {
  if (auto Index = getExistingIndex(VD))
    return *Index;

  unsigned Index = Desc.size();
  return Desc[VD] = Index;
}
