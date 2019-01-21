; RUN: opt -analyze -bta %s 2>&1 | FileCheck %s --implicit-check-not="; stage"

define stage(1) i32 @f(i1 stage(1) %b, i32 stage(1) %x) stage(1) {
; CHECK-LABEL: {{^}}entry:
  entry:                              ; CHECK-NEXT: stage(0)
  br i1 %b, label %left, label %right ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}left:
left:                           ; CHECK-NEXT: stage(1)
  %lval = add i32 %x, 1         ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}right:
right:                          ; CHECK-NEXT: stage(1)
  %rval = sub i32 %x, 1         ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}exit:
exit:                           ; CHECK-NEXT: stage(0)
  %val = phi i32 [ %lval, %left ], [ %rval, %right ] ; CHECK-NEXT: stage(0)
  ret i32 %val                                       ; CHECK-NEXT: stage(1)
}
