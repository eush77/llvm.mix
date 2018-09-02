; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define void @f()
; CHECK-STAGE-LABEL: define void @f()
define void @f() stage(1) {
  ; CHECK-NEXT: ret void
  ; CHECK-STAGE-NEXT: ret void
  ret void
}

; CHECK-LABEL: define void @main()
define void @main() {
  ; CHECK-NEXT: %context = call i8* @LLVMContextCreate()
  %context = call i8* @LLVMContextCreate()
  ; CHECK: [[context:%.+]] = bitcast i8* %context to %struct.LLVMOpaqueContext*
  ; CHECK: [[module:%.+]] = call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithNameInContext({{.*}}, %struct.LLVMOpaqueContext* [[context]])
  ; CHECK: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction(%struct.LLVMOpaqueModule* [[module]],
  ; CHECK: [[builder:%.+]] = call %struct.LLVMOpaqueBuilder* @LLVMCreateBuilderInContext(%struct.LLVMOpaqueContext* [[context]])
  ; CHECK: [[entry:%.+]] = call %struct.LLVMOpaqueBasicBlock* @LLVMAppendBasicBlockInContext(%struct.LLVMOpaqueContext* [[context]], %struct.LLVMOpaqueValue* [[function]],
  ; CHECK: call void @LLVMPositionBuilderAtEnd(%struct.LLVMOpaqueBuilder* [[builder]], %struct.LLVMOpaqueBasicBlock* [[entry]])
  ; CHECK: call %struct.LLVMOpaqueValue* @LLVMBuildRetVoid(%struct.LLVMOpaqueBuilder* [[builder]])
  ; CHECK: call void @LLVMDisposeBuilder(%struct.LLVMOpaqueBuilder* [[builder]])
  ; CHECK: %function = bitcast %struct.LLVMOpaqueValue* [[function]] to i8*
  %function = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (void ()* @f to i8*), i8* %context)
  ; CHECK-NEXT: call void @LLVMDumpValue(i8* %function)
  call void @LLVMDumpValue(i8* %function)
  ; CHECK-NEXT: call void @LLVMContextDispose(i8* %context)
  call void @LLVMContextDispose(i8* %context)
  ; CHECK-NEXT: ret void
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
