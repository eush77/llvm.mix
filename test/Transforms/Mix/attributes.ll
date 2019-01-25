; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; FIXME: Require zeroext on the stage(2) function

; STAGE0: define private stage(1) %struct.LLVMOpaqueValue* @f.mix
define stage(2) zeroext i32 @f(i32 stage(2)) stage(2) {
  ret i32 1
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i32)* @f to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
