; RUN: opt -S -mix %s -o - | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

define void @f() stage(1) {
  ret void
}

; STAGE0-LABEL: define void @main()
define void @main() {
  %context = call %struct.LLVMOpaqueContext* @LLVMContextCreate()
  ; STAGE0: %c = bitcast %struct.LLVMOpaqueContext* %context to i8*
  %c = bitcast %struct.LLVMOpaqueContext* %context to i8*
  ; STAGE0-NEXT: [[context:%.+]] = bitcast i8* %c to %struct.LLVMOpaqueContext*
  ; STAGE0-NEXT: [[function:%.+]] = call %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* [[context]])
  ; STAGE0-NEXT: %f = bitcast %struct.LLVMOpaqueValue* [[function]] to i8*
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void ()* @f to i8*), i8* %c)
  ; STAGE0-NEXT: %function = bitcast i8* %f to %struct.LLVMOpaqueValue*
  %function = bitcast i8* %f to %struct.LLVMOpaqueValue*
  call void @LLVMDumpValue(%struct.LLVMOpaqueValue* %function)
  call void @LLVMContextDispose(%struct.LLVMOpaqueContext* %context)
  ret void
}

%struct.LLVMOpaqueContext = type opaque
%struct.LLVMOpaqueValue = type opaque

declare i8* @llvm.mix(i8*, i8*, ...)
declare %struct.LLVMOpaqueContext* @LLVMContextCreate()
declare void @LLVMContextDispose(%struct.LLVMOpaqueContext*)
declare %struct.LLVMOpaqueValue* @LLVMModuleCreateWithNameInContext(i8*, %struct.LLVMOpaqueContext*)
declare void @LLVMDumpValue(%struct.LLVMOpaqueValue*)
