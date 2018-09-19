; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: Wrong types {{.*}}
; CHECK-NEXT: void (i8, i32)* @llvm.object.stage.i8
declare void @llvm.object.stage.i8(i8, i32)

declare void @llvm.object.stage.p0i32(i32*, i32)

define void @f(i32* %p, i32 %x) {
  ; CHECK: Stage argument of llvm.object.stage must be a constant int
  ; CHECK-NEXT: call void @llvm.object.stage.p0i32(i32* %p, i32 %x)
  call void @llvm.object.stage.p0i32(i32* %p, i32 %x)
  ; CHECK: Stage argument of llvm.object.stage must be non-negative
  ; CHECK-NEXT: call void @llvm.object.stage.p0i32(i32* %p, i32 -1)
  call void @llvm.object.stage.p0i32(i32* %p, i32 -1)
  ret void
}

; CHECK: {{.*}} error: input module is broken!
