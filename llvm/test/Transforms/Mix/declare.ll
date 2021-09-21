; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-DAG: [[g:@.+]] = {{.*}} c"g\00"
; STAGE0-DAG: @LLVMAddFunction({{.*}} [[g]], {{.*}})
; STAGE1: declare float @g(float)
declare float @g(float)

; STAGE0-DAG: [[sqrt:@.+]] = {{.*}} c"llvm.sqrt.f32\00"
; STAGE0-DAG: @LLVMAddFunction({{.*}} [[sqrt]], {{.*}})
; STAGE1: declare float @llvm.sqrt.f32(float)
declare float @llvm.sqrt.f32(float)

; STAGE0-DAG: [[casted:@.+]] = {{.*}} c"casted\00"
; STAGE0-DAG: @LLVMAddFunction({{.*}} [[casted]], {{.*}})
; STAGE1: declare i32 @casted(float)
declare i32 @casted(float)

; STAGE1-LABEL: define dso_local float @f(float %x)
define stage(1) float @f(float stage(1) %x) stage(1) {
  ; STAGE1: %y = call float @g(float %x)
  %y = call float @g(float %x)
  ; STAGE1: %z = call float @g(float %y)
  %z = call float @g(float %y)
  ; STAGE1: %a = call float @llvm.sqrt.f32(float %z)
  %a = call float @llvm.sqrt.f32(float %z)
  ; STAGE1: %b = call float bitcast (i32 (float)* @casted to float (float)*)(float %a)
  %b = call float bitcast (i32 (float)* @casted to float (float)*)(float %a)
  ret float %b
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (float (float)* @f to i8*), i8* %c)
  %m = call i8* @LLVMGetGlobalParent(i8* %f)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
