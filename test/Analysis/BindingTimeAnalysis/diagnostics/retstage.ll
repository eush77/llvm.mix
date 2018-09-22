; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: error: Inferred stage contradicts the declared return stage(0) of @retstage:
; CHECK: ret i32 %x  ; stage(1), in %entry
define i32 @retstage(i32 stage(1) %x) stage(1) {
entry:
  ret i32 %x
}
