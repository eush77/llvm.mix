; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare void @f(i32) stage(1)

; CHECK: error: Inferred argument stage(1) contradicts the declared parameter stage of @f:
; CHECK: call void @f(i32 %x)  ; in %entry
define void @argstage(i32 stage(1) %x) stage(1) {
entry:
  call void @f(i32 %x)
  ret void
}
