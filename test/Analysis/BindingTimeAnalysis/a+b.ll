; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=static --implicit-check-not=sbb

; CHECK-LABEL: define i32 @plus(i32 %x, i32 %y)
define i32 @plus(i32 %x, i32 %y) {
  ; CHECK: add {{.*}}; static
  %z = add i32 %x, %y
  ret i32 %z
}

; CHECK-LABEL: define i32 @inc(i32 %x)
define i32 @inc(i32 %x) {
  ; CHECK: add {{.*}}; static
  %y = add i32 %x, 1
  ret i32 %y
}
