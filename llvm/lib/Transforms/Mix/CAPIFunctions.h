//===-- CAPIFunctions.h - C API Function Definitions ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines interface to C API functions for code generation.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_CAPIFUNCTIONS_H
#define LLVM_LIB_TRANSFORMS_MIX_CAPIFUNCTIONS_H

#include "Types.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"

namespace llvm {

class Constant;

namespace mix {

#define CONTEXT M.getContext()
#define HANDLE_API_FUNCTION(Name, Result, ...)                                 \
  inline FunctionCallee get##Name##Fn(Module &M) {                             \
    return M.getOrInsertFunction(                                              \
        "LLVM" #Name, FunctionType::get(Result, {__VA_ARGS__}, false));        \
  }
#include "CAPIFunctions.def"
#undef HANDLE_API_FUNCTION
#undef CONTEXT

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_CAPIFUNCTIONS_H
