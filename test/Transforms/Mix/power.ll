; RUN: opt -S -mix %s -o - | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0: define private %struct.LLVMOpaqueValue* @power-iter.main(%struct.LLVMOpaqueContext* %context, i32 %n)
; STAGE0: define private %struct.LLVMOpaqueValue* @power-iter.mix(i8** %mix.context, i32 %n)
; STAGE1-LABEL: define dso_local i32 @power-iter(i32 %x)
define stage(1) i32 @power-iter(i32 stage(1) %x, i32 %n) stage(1) {
; STAGE0: entry:
entry:
  ; STAGE0: br {{.*}} %check-next
  br label %check-next

; STAGE0: check-next:
check-next:
  ; STAGE0: %res.0 = phi
  %res.0 = phi i32 [ 1, %entry ], [ %res.1, %next ]
  ; STAGE0: %n.0 = phi
  %n.0 = phi i32 [ %n, %entry ], [ %n.1, %next ]
  ; STAGE0: %zerop = icmp eq
  %zerop = icmp eq i32 %n.0, 0
  ; STAGE0: br {{.*}} %exit, {{.*}} %next
  br i1 %zerop, label %exit, label %next

; STAGE0: next:
next:
  ; STAGE1: [[p1:%res\.[0-9]+]] = mul i32 1, %x
  ; STAGE1: [[p2:%res\.[0-9]+]] = mul i32 [[p1]], %x
  ; STAGE1: [[p3:%res\.[0-9]+]] = mul i32 [[p2]], %x
  ; STAGE1: [[p4:%res\.[0-9]+]] = mul i32 [[p3]], %x
  ; STAGE1: [[p5:%res\.[0-9]+]] = mul i32 [[p4]], %x
  %res.1 = mul i32 %res.0, %x
  ; STAGE0: %n.1 = sub
  %n.1 = sub i32 %n.0, 1
  ; STAGE0: br {{.*}} %check-next
  br label %check-next

; STAGE0: exit:
exit:
  ; STAGE1: ret
  ret i32 %res.0
}

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
