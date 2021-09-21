; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: @select.mix
; STAGE1-LABEL: @select
define stage(1) i32 @select(i1 stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildSelect
  ; STAGE1: select i1 %x, i32 0, i32 1
  %t = select i1 %x, i32 0, i32 1
  ret i32 %t
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %select = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1)* @select to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %select)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
