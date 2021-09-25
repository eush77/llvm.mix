; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: Attribute 'nocapture' applied to incompatible type!
; CHECK-NEXT: i8 (i8, i32)* @llvm.object.stage.i8
declare i8 @llvm.object.stage.i8(i8, i32)

declare i32* @llvm.object.stage.p0i32(i32*, i32)

define void @f(i32* %p, i32 %x) {
  ; CHECK: Stage argument of llvm.object.stage must be a constant int
  ; CHECK-NEXT: %p0 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 %x)
  %p0 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 %x)
  ; CHECK: Stage argument of llvm.object.stage must be non-negative
  ; CHECK-NEXT: %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 -1)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 -1)
  ret void
}

; CHECK: {{.*}} error: input module is broken!
