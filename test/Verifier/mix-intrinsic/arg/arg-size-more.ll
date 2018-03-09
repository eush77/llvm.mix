; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s

declare i8* @llvm.mix(i8*, metadata, ...)

define void @g(i32 %x) {
  ; CHECK: too many arguments
  %m = call i8* (i8*, metadata, ...) @llvm.mix(i8* null, metadata !"g", i32 1, i32 2)
  ret void
}
