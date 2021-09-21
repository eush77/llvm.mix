; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: @gep.mix
; STAGE1-LABEL: @gep
define stage(1) i32* @gep([4 x i32]* stage(1) %x, i32 stage(1) %y, i32 stage(1) %z) stage(1) {
  ; STAGE0: @LLVMBuildGEP
  ; STAGE1: getelementptr [4 x i32], [4 x i32]* %x, i32 %y, i32 %z
  %t = getelementptr [4 x i32], [4 x i32]* %x, i32 %y, i32 %z
  ret i32* %t
}

; STAGE0-LABEL: @inboundsgep.mix
; STAGE1-LABEL: @inboundsgep
define stage(1) i32* @inboundsgep({ i32, { i32, i32 } }* stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildInBoundsGEP
  ; STAGE1: getelementptr inbounds { i32, { i32, i32 } }, { i32, { i32, i32 } }* %x, i32 0, i32 1, i32 0
  %t = getelementptr inbounds { i32, { i32, i32 } }, { i32, { i32, i32 } }* %x, i32 0, i32 1, i32 0
  ret i32* %t
}

; STAGE0-LABEL: @structgep.mix
; STAGE1-LABEL: @structgep
define stage(1) i32* @structgep({ i32, i32 }* stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildStructGEP
  ; STAGE1: getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 1
  %t = getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 1
  ret i32* %t
}

; STAGE0-LABEL: @structgep.i64.mix
; STAGE1-LABEL: @structgep.i64
define stage(1) i32* @structgep.i64([2 x i32]* stage(1) %x) stage(1) {
  ; STAGE0: @LLVMBuildStructGEP
  ; STAGE1: getelementptr inbounds [2 x i32], [2 x i32]* %x, i32 0, i32 0
  %t = getelementptr inbounds [2 x i32], [2 x i32]* %x, i64 0, i64 0
  ret i32* %t
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %gep = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32* ([4 x i32]*, i32, i32)* @gep to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %gep)
  %inboundsgep = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32* ({ i32, { i32, i32 } }*)* @inboundsgep to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %inboundsgep)
  %structgep = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32* ({ i32, i32 }*)* @structgep to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %structgep)
  %structgep.i64 = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32* ([2 x i32]*)* @structgep.i64 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %structgep.i64)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
