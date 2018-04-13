; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=static --implicit-check-not=sbb

; CHECK-LABEL: define void @dynamic-block(i1* %pb)
define void @dynamic-block(i1* %pb) {
entry:
  br label %header

header:
  %n.0 = phi i32 [ 0, %entry ], [ %n.1, %header]
  %n.1 = add i32 %n.0, 1
  %b = load i1, i1* %pb
  br i1 %b, label %exit, label %header

exit:
  ret void
}
