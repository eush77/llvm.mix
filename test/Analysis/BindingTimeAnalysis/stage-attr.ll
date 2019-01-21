; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

; CHECK-LABEL: Function Attrs: stage(2)
; CHECK-NEXT: @f(i32 %x, i32 stage(1) %y, i32 stage(2) %z)
define void @f(i32 stage(0) %x, i32 stage(1) %y, i32 stage(2) %z) stage(2) {
; CHECK-NEXT: {{^}}; stage(0)
  %x2 = mul i32 %x, %x          ; CHECK-NEXT: stage(0)
  %y2 = mul i32 %y, %y          ; CHECK-NEXT: stage(1)
  %z2 = mul i32 %z, %z          ; CHECK-NEXT: stage(2)
  %xy = mul i32 %x, %y          ; CHECK-NEXT: stage(1)
  %yz = mul i32 %y, %z          ; CHECK-NEXT: stage(2)
  %xz = mul i32 %x, %z          ; CHECK-NEXT: stage(2)
  ret void                      ; CHECK-NEXT: stage(2)
}
