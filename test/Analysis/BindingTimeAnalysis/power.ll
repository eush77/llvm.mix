; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

; CHECK-LABEL: Function Attrs: stage(1)
; CHECK-NEXT: define stage(1) i32 @power-iter(i32 stage(1) %x, i32 %n)
define stage(1) i32 @power-iter(i32 stage(1) %x, i32 %n) stage(1) {
; CHECK-LABEL: {{^}}entry:
entry:                          ; CHECK-NEXT: stage(0)
  br label %check-next          ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}check-next:
check-next:                                         ; CHECK-NEXT: stage(0)
  %res.0 = phi i32 [ 1, %entry ], [ %res.1, %next ] ; CHECK-NEXT: stage(0)
  %n.0 = phi i32 [ %n, %entry ], [ %n.1, %next ]    ; CHECK-NEXT: stage(0)
  %zerop = icmp eq i32 %n.0, 0                      ; CHECK-NEXT: stage(0)
  br i1 %zerop, label %exit, label %next            ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}next:
next:                           ; CHECK-NEXT: stage(0)
  %res.1 = mul i32 %res.0, %x   ; CHECK-NEXT: stage(1)
  %n.1 = sub i32 %n.0, 1        ; CHECK-NEXT: stage(0)
  br label %check-next          ; CHECK-NEXT: stage(0)

; CHECK: exit:
exit:                           ; CHECK-NEXT: stage(0)
  ret i32 %res.0                ; CHECK-NEXT: stage(1)
}
