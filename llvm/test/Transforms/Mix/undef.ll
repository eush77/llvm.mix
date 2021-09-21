; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

define i32 @f() stage(1) {
  ; STAGE0: [[undef:%.+]] = call %struct.LLVMOpaqueValue* @LLVMGetUndef
  ; STAGE0: @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* [[undef]])
  ; STAGE1: ret i32 undef
  ret i32 undef
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 ()* @f to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
