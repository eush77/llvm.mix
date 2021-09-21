; RUN: opt -S -mix %s -o - \
; RUN: | FileCheck %s --check-prefix=STAGE0 --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0-LABEL: define void @f()
; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* %context)
; STAGE0: call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithNameInContext
; STAGE0: [[context:%.+]] = alloca i8*
; STAGE0: call %struct.LLVMOpaqueBuilder* @LLVMCreateBuilderInContext
; STAGE0: call %struct.LLVMOpaqueValue* @f.mix(i8** [[context]])
; STAGE0: call void @LLVMDisposeBuilder

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.mix(i8** %mix.context)
; STAGE1-LABEL: define dso_local void @f()
define void @f() stage(1) {
  ; STAGE0: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction
  ; STAGE0: [[entry:%.+]] = call %struct.LLVMOpaqueBasicBlock* @LLVMAppendBasicBlockInContext(%struct.LLVMOpaqueContext* {{.*}}, %struct.LLVMOpaqueValue* [[function]],
  ; STAGE0: call void @LLVMPositionBuilderAtEnd(%struct.LLVMOpaqueBuilder* {{.*}}, %struct.LLVMOpaqueBasicBlock* [[entry]])
  ; STAGE0: call %struct.LLVMOpaqueValue* @LLVMBuildRetVoid
  ; STAGE1-NEXT: ret void
  ret void
}

; STAGE0-LABEL: define void @main()
define void @main() {
  ; STAGE0-NEXT: %context = call i8* @LLVMContextCreate()
  %context = call i8* @LLVMContextCreate()
  ; STAGE0-NEXT: [[context:%.+]] = bitcast i8* %context to %struct.LLVMOpaqueContext*
  ; STAGE0-NEXT: [[function:%.+]] = call %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* [[context]])
  ; STAGE0: %function = bitcast %struct.LLVMOpaqueValue* [[function]] to i8*
  %function = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void ()* @f to i8*), i8* %context)
  ; STAGE0-NEXT: call void @LLVMDumpValue(i8* %function)
  call void @LLVMDumpValue(i8* %function)
  ; STAGE0-NEXT: call void @LLVMContextDispose(i8* %context)
  call void @LLVMContextDispose(i8* %context)
  ; STAGE0-NEXT: ret void
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
