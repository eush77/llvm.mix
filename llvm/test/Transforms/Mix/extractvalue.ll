; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: @struct.mix
; STAGE1-LABEL: @struct
define stage(1) i32 @struct({ i32 } stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildExtractValue
  ; STAGE1: extractvalue { i32 } %x, 0
  %t = extractvalue { i32 } %x, 0
  ret i32 %t
}

; STAGE0-LABEL: @array.mix
; STAGE1-LABEL: @array
define stage(1) i32 @array([1 x i32] stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildExtractValue
  ; STAGE1: extractvalue [1 x i32] %x, 0
  %t = extractvalue [1 x i32] %x, 0
  ret i32 %t
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %struct = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 ({ i32 })* @struct to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct)
  %array = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 ([1 x i32])* @array to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %array)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
