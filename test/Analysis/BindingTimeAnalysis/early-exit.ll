; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

; CHECK-LABEL: Function Attrs: stage(1)
; CHECK-NEXT: define stage(1) i32 @f(i1 stage(1) %p)
define stage(1) i32 @f(i1 stage(1) %p) stage(1) {
; CHECK-LABEL: {{^}}entry:
  entry:                                     ; CHECK-NEXT: stage(0)
  br i1 %p, label %early-exit, label %branch ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}early-exit:
early-exit:                     ; CHECK-NEXT: stage(1)
  ret i32 1                     ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}branch:
branch:                         ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}exit:
exit:                           ; CHECK-NEXT: stage(0)
  ret i32 0                     ; CHECK-NEXT: stage(1)
}
