; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

define i32 @e(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; CHECK-LABEL: Function Attrs: stage(1)
; CHECK-NEXT: define i32 @s(i32 %x)
define i32 @s(i32 %x) stage(1) {
; CHECK-NEXT: {{^}}; stage(0)
  %y = add i32 %x, 1            ; CHECK-NEXT: stage(0)
  ret i32 %y                    ; CHECK-NEXT: stage(0)
}

; CHECK-LABEL: Function Attrs: stage(1)
; CHECK-NEXT: define stage(1) i32 @g(i32 %x, i32 (i32)* %f0)
define stage(1) i32 @g(i32 %x, i32 (i32)* %f0) stage(1) {
; CHECK-NEXT: {{^}}; stage(0)
  %t0 = call i32 @s(i32 %x)     ; CHECK-NEXT: stage(0)
  %t1 = call i32 @e(i32 %t0)    ; CHECK-NEXT: stage(1)
  %t2 = call i32 %f0(i32 %x)    ; CHECK-NEXT: stage(1)
  ret i32 %t2                   ; CHECK-NEXT: stage(1)
}
