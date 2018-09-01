; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define stage(1) i32 @power-iter(i32 stage(1) %x, i32 %n)
; CHECK-STAGE-LABEL: define i32 @power-iter(i32 %x)
define stage(1) i32 @power-iter(i32 stage(1) %x, i32 %n) stage(1) {
; CHECK: entry:
entry:
  ; CHECK: br {{.*}} %check-next
  br label %check-next

; CHECK: check-next:
check-next:
  ; CHECK: %res.0 = phi
  %res.0 = phi i32 [ 1, %entry ], [ %res.1, %next ]
  ; CHECK: %n.0 = phi
  %n.0 = phi i32 [ %n, %entry ], [ %n.1, %next ]
  ; CHECK: %zerop = icmp eq
  %zerop = icmp eq i32 %n.0, 0
  ; CHECK: br {{.*}} %exit, {{.*}} %next
  br i1 %zerop, label %exit, label %next

; CHECK: next:
next:
  ; CHECK-STAGE: [[p1:%res\.[0-9]+]] = mul i32 1, %x
  ; CHECK-STAGE: [[p2:%res\.[0-9]+]] = mul i32 [[p1]], %x
  ; CHECK-STAGE: [[p3:%res\.[0-9]+]] = mul i32 [[p2]], %x
  ; CHECK-STAGE: [[p4:%res\.[0-9]+]] = mul i32 [[p3]], %x
  ; CHECK-STAGE: [[p5:%res\.[0-9]+]] = mul i32 [[p4]], %x
  %res.1 = mul i32 %res.0, %x
  ; CHECK: %n.1 = sub
  %n.1 = sub i32 %n.0, 1
  ; CHECK: br {{.*}} %check-next
  br label %check-next

; CHECK: exit:
exit:
  ; CHECK-STAGE: ret
  ret i32 %res.0
}

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32, i32)* @power-iter to i8*), i8* %c, i32 5)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
