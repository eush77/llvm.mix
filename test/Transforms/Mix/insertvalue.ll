; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: @struct.mix
; STAGE1-LABEL: @struct
define stage(1) { i32 } @struct(i32 stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildInsertValue
  ; STAGE1: insertvalue { i32 } undef, i32 %x, 0
  %t = insertvalue { i32 } undef, i32 %x, 0
  ret { i32 } %t
}

; STAGE0-LABEL: @array.mix
; STAGE1-LABEL: @array
define stage(1) [1 x i32] @array(i32 stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildInsertValue
  ; STAGE1: insertvalue [1 x i32] undef, i32 %x, 0
  %t = insertvalue [1 x i32] undef, i32 %x, 0
  ret [1 x i32] %t
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %struct = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast ({ i32 } (i32)* @struct to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct)
  %array = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast ([1 x i32] (i32)* @array to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %array)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
