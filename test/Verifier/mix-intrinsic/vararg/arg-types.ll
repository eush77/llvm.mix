; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s

declare i8* @llvm.mix(i8*, metadata, ...)

define void @g(i32 %x, ...) {
  ; CHECK: does not match signature of the source function
  %f = call i8* (i8*, metadata, ...) @llvm.mix(i8* null, metadata !"g", i1 1, i1 1)
  ret void
}
