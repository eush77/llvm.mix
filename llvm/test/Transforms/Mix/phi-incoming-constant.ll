; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0: call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 0, i32 0)
; STAGE0: call void @LLVMAddIncoming
; STAGE0: call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 1, i32 0)
; STAGE0: call void @LLVMAddIncoming
define void @f(i1 stage(1) %x) stage(1) {
entry:
  br i1 %x, label %exit, label %head

head:
  br label %exit

exit:
  ; STAGE1: %y = phi i32 [ 0, %entry ], [ 1, %head ]
  %y = phi i32 [ 0, %entry ], [ 1, %head ]
  ret void
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i1)* @f to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
