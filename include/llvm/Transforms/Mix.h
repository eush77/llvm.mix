//===- Mix.h - Runtime Specialization Pass ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
///
/// This file defines the interface to add MixPass to a pass manager pipeline.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_MIX_H
#define LLVM_TRANSFORMS_MIX_H

namespace llvm {

class PassManagerBuilder;

/// Add Mix pass to the appropriate extension point.
void addMixPass(PassManagerBuilder &Builder);

}

#endif // LLVM_TRANSFORMS_MIX_H
