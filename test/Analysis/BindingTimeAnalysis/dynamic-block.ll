; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

; CHECK-LABEL: Function Attrs: stage(1)
; CHECK-NEXT: define void @dynamic-block(i1 stage(1) %b)
define void @dynamic-block(i1 stage(1) %b) stage(1) {
; CHECK-LABEL: {{^}}entry:
entry:                          ; CHECK-NEXT: stage(0)
  br label %header              ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}header:
header:                                          ; CHECK-NEXT: stage(1)
  %n.0 = phi i32 [ 0, %entry ], [ %n.1, %header] ; CHECK-NEXT: stage(1)
  %n.1 = add i32 %n.0, 1                         ; CHECK-NEXT: stage(1)
  br i1 %b, label %exit, label %header           ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}exit:
exit:                           ; CHECK-NEXT: stage(1)
  ret void                      ; CHECK-NEXT: stage(1)
}
