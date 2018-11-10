; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define void @f()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* %context)
; CHECK: call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithNameInContext
; CHECK: [[context:%.+]] = alloca i8*
; CHECK: call %struct.LLVMOpaqueBuilder* @LLVMCreateBuilderInContext
; CHECK: call %struct.LLVMOpaqueValue* @f.mix(i8** [[context]])
; CHECK: call void @LLVMDisposeBuilder

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @f.mix(i8** %mix.context)
; CHECK-STAGE-LABEL: define void @f()
define void @f() stage(1) {
  ; CHECK: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction
  ; CHECK: [[entry:%.+]] = call %struct.LLVMOpaqueBasicBlock* @LLVMAppendBasicBlockInContext(%struct.LLVMOpaqueContext* {{.*}}, %struct.LLVMOpaqueValue* [[function]],
  ; CHECK: call void @LLVMPositionBuilderAtEnd(%struct.LLVMOpaqueBuilder* {{.*}}, %struct.LLVMOpaqueBasicBlock* [[entry]])
  ; CHECK: call %struct.LLVMOpaqueValue* @LLVMBuildRetVoid
  ; CHECK-STAGE-NEXT: ret void
  ret void
}

; CHECK-LABEL: define void @main()
define void @main() {
  ; CHECK-NEXT: %context = call i8* @LLVMContextCreate()
  %context = call i8* @LLVMContextCreate()
  ; CHECK-NEXT: [[context:%.+]] = bitcast i8* %context to %struct.LLVMOpaqueContext*
  ; CHECK-NEXT: [[function:%.+]] = call %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* [[context]])
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
