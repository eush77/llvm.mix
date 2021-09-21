; RUN: opt -analyze -bta %s | FileCheck %s --implicit-check-not="; stage"

; CHECK-LABEL: @without.phis
define void @without.phis(i1 stage(1) %b, i32 stage(1) %x) stage(1) {
; CHECK-LABEL: {{^}}entry:
  entry:                              ; CHECK-NEXT: stage(0)
  br i1 %b, label %left, label %right ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}left:
left:                           ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}right:
right:                          ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(0)

; CHECK-LABEL: {{^}}exit:
exit:                           ; CHECK-NEXT: stage(0)
  ret void                      ; CHECK-NEXT: stage(1)
}

; CHECK-LABEL: @with.phis
define stage(1) i32 @with.phis(i1 stage(1) %b, i32 stage(1) %x) stage(1) {
; CHECK-LABEL: {{^}}entry:
  entry:                              ; CHECK-NEXT: stage(0)
  br i1 %b, label %left, label %right ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}left:
left:                           ; CHECK-NEXT: stage(1)
  %lval = add i32 %x, 1         ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}right:
right:                          ; CHECK-NEXT: stage(1)
  %rval = sub i32 %x, 1         ; CHECK-NEXT: stage(1)
  br label %exit                ; CHECK-NEXT: stage(1)

; CHECK-LABEL: {{^}}exit:
exit:                           ; CHECK-NEXT: stage(1)
  %val = phi i32 [ %lval, %left ], [ %rval, %right ] ; CHECK-NEXT: stage(1)
  ret i32 %val                                       ; CHECK-NEXT: stage(1)
}
