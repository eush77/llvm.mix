; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define void @f(i32 %x)
; CHECK-STAGE-LABEL: define void @f()
define void @f(i32 %x) {
  ; CHECK-NEXT: ret void
  ; CHECK-STAGE-NEXT: ret void
  ret void
}

; CHECK: define private %struct.LLVMOpaqueModule* @f.mix(%struct.LLVMOpaqueContext* [[context:%.+]], i32 %x)
; CHECK: [[module:%.+]] = call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithNameInContext({{.*}}, %struct.LLVMOpaqueContext* [[context]])
; CHECK: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction(%struct.LLVMOpaqueModule* [[module]],
; CHECK: [[builder:%.+]] = call %struct.LLVMOpaqueBuilder* @LLVMCreateBuilderInContext(%struct.LLVMOpaqueContext* [[context]])
; CHECK: [[entry:%.+]] = call %struct.LLVMOpaqueBasicBlock* @LLVMAppendBasicBlockInContext(%struct.LLVMOpaqueContext* [[context]], %struct.LLVMOpaqueValue* [[function]],
; CHECK: call void @LLVMPositionBuilderAtEnd(%struct.LLVMOpaqueBuilder* [[builder]], %struct.LLVMOpaqueBasicBlock* [[entry]])
; CHECK: call %struct.LLVMOpaqueValue* @LLVMBuildRetVoid(%struct.LLVMOpaqueBuilder* [[builder]])
; CHECK: call void @LLVMDisposeBuilder(%struct.LLVMOpaqueBuilder* [[builder]])
; CHECK: ret %struct.LLVMOpaqueModule* [[module]]

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; CHECK: call %struct.LLVMOpaqueModule* @f.mix({{.*}}, i32 1)
  %m = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (void (i32)* @f to i8*), i8* %c, i32 1)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
