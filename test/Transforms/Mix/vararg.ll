; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1 --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE1-LABEL: define dso_local void @f()
define void @f(i32 %a, i32 %b) stage(1) {
  ; STAGE1: call i32 (i8*, ...) @printf(i8* inttoptr ({{.*}}), i32 4, i32 6)
  %_ = call i32(i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @fmt, i32 0, i32 0), i32 %a, i32 %b)
  ret void
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (void (i32, i32)* @f to i8*), i8* %c, i32 4, i32 6)
  %m = call i8* @LLVMGetGlobalParent(i8* %f)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

@fmt = constant [8 x i8] c"%d, %d\0A\00"

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
declare void @LLVMDumpModule(i8*)
declare void @LLVMDumpValue(i8*)
declare i32 @printf(i8*, ...)
