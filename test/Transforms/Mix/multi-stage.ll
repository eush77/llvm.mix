; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0: @f.mix.mix(i8** %mix.context, i32 %x)
; STAGE1: @f.mix(i8** %mix.context, i32 %y)
define void @f(i32 stage(0) %x, i32 stage(1) %y, i32 stage(2) %z) stage(2) {
  ; STAGE0: %x1 = mul i32 %x, %x
  ; STAGE1: [[x1:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 16,
  %x1 = mul i32 %x, %x
  ; STAGE0: %y1 = call %struct.LLVMOpaqueValue* @LLVMBuildBinOp
  ; STAGE1: %y1 = mul i32 %y, 16
  ; STAGE1: [[y1_i64:%.+]] = zext i32 %y1 to i64
  ; STAGE1: [[y1:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 [[y1_i64]],
  %y1 = mul i32 %y, %x1
  ; STAGE0: %z1 = call %struct.LLVMOpaqueValue* @LLVMBuildCall({{.*}}LLVMBuildBinOp
  ; STAGE1: %z1 = call %struct.LLVMOpaqueValue* @LLVMBuildBinOp({{.*}}, %struct.LLVMOpaqueValue* %z, %struct.LLVMOpaqueValue* [[x1]],
  %z1 = mul i32 %z, %x1
  ; STAGE0: %z2 = call %struct.LLVMOpaqueValue* @LLVMBuildCall({{.*}}LLVMBuildBinOp
  ; STAGE1: %z2 = call %struct.LLVMOpaqueValue* @LLVMBuildBinOp({{.*}}, %struct.LLVMOpaqueValue* %z, %struct.LLVMOpaqueValue* [[y1]],
  %z2 = mul i32 %z, %y1
  ret void
}

define void @main() {
  %context = call i8* @LLVMContextCreate()
  %function = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, i32, i32)* @f to i8*), i8* %context, i32 4)
  %module = call i8* @LLVMGetGlobalParent(i8* %function)
  call void @LLVMDumpModule(i8* %module)
  call void @LLVMContextDispose(i8* %context)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
