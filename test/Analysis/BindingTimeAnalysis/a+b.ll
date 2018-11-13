; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=stage

; CHECK-LABEL: define i32 @plus(i32 %x, i32 %y)
define i32 @plus(i32 %x, i32 %y) {
; CHECK-NEXT: stage(0)
  %z = add i32 %x, %y           ; CHECK-NEXT: stage(0)
  ret i32 %z                    ; CHECK-NEXT: stage(1)
}

; CHECK-LABEL: define i32 @inc(i32 %x)
define i32 @inc(i32 %x) {
; CHECK-NEXT: stage(0)
  %y = add i32 %x, 1            ; CHECK-NEXT: stage(0)
  ret i32 %y                    ; CHECK-NEXT: stage(1)
}
