; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not=static --implicit-check-not=sbb

; CHECK-LABEL: define i32 @power-iter(i32* %px, i32 %n)
define i32 @power-iter(i32* %px, i32 %n) {
entry:
  %x = load i32, i32* %px
  ; CHECK: br label %check-next ; static
  ; CHECK: sbb = %entry{{$}}
  br label %check-next

; CHECK: check-next:
; CHECK-NEXT: {{^}}; static
check-next:
  ; CHECK: %res.0 = phi {{.*}}; static
  %res.0 = phi i32 [ 1, %entry ], [ %res.1, %next ]
  ; CHECK: %n.0 = phi {{.*}}; static
  %n.0 = phi i32 [ %n, %entry ], [ %n.1, %next ]
  ; CHECK: %zerop = icmp eq {{.*}}; static
  %zerop = icmp eq i32 %n.0, 0
  ; CHECK: br {{.*}}, label %exit, label %next ; static
  ; CHECK: sbb = %check-next{{$}}
  br i1 %zerop, label %exit, label %next

; CHECK: next:
; CHECK-NEXT: {{^}}; static
next:
  %res.1 = mul i32 %res.0, %x
  ; CHECK: %n.1 = sub {{.*}}; static
  %n.1 = sub i32 %n.0, 1
  ; CHECK: br label %check-next ; static
  ; CHECK: sbb = %next{{$}}
  br label %check-next

; CHECK: exit:
; CHECK-NEXT: {{^}}; static
exit:
  ret i32 %res.0
}
