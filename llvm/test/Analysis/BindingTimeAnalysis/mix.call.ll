; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not="; stage"

define stage(1) i8* @stage1(i32 %x) stage(1) {
; CHECK-LABEL: {{^}}stage1:
stage1:                         ; CHECK-NEXT: stage(0)
  %f = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32)* @f to i8*), i32 %x) ; CHECK-NEXT: stage(1)
  ret i8* %f                    ; CHECK-NEXT: stage(1)
}

define stage(1) i8* @stage2(i32 stage(1) %x) stage(2) {
; CHECK-LABEL: {{^}}stage2:
stage2:                         ; CHECK-NEXT: stage(0)
  %f = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32)* @f to i8*), i32 4) ; CHECK-NEXT: stage(1)
  ret i8* %f                    ; CHECK-NEXT: stage(1)
}

define void @f(i32) stage(1) {
; CHECK-LABEL: {{^}}f:
f:                              ; CHECK-NEXT: stage(0)
  ret void                      ; CHECK-NEXT: stage(1)
}

declare i8* @llvm.mix.call(i8*, ...)
