; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s

declare i8* @llvm.mix(i8*, metadata, ...)

define void @g(i32 %x) {
  ; CHECK: function-id argument {{.*}} must be a metadata string
  %m = call i8* (i8*, metadata, ...) @llvm.mix(i8* null, metadata !0, i32 1)
  ret void
}

!0 = !{}
