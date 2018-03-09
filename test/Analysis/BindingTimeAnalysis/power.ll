; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=static

; CHECK-LABEL: define i32 @power-iter(i32* %px, i32 %n)
define i32 @power-iter(i32* %px, i32 %n) {
entry:
  %x = load i32, i32* %px
  br label %check-next

check-next:
  %res.0 = phi i32 [ 1, %entry ], [ %res.1, %next ]
  ; CHECK: %n.0 = phi {{.*}}; static
  %n.0 = phi i32 [ %n, %entry ], [ %n.1, %next ]
  ; CHECK: %zerop = icmp eq {{.*}}; static
  %zerop = icmp eq i32 %n.0, 0
  br i1 %zerop, label %exit, label %next

next:
  %res.1 = mul i32 %res.0, %x
  ; CHECK: %n.1 = sub {{.*}}; static
  %n.1 = sub i32 %n.0, 1
  br label %check-next

exit:
  ret i32 %res.0
}
