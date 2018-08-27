; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define void @f(
; CHECK-STAGE-LABEL: define void @f()
define void @f() {
  ; CHECK-NEXT: ret void
  ; CHECK-STAGE-NEXT: ret void
  ret void
}

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call %struct.LLVMOpaqueContext* @LLVMContextCreate()
  ; CHECK: %c = bitcast %struct.LLVMOpaqueContext* %context to i8*
  %c = bitcast %struct.LLVMOpaqueContext* %context to i8*
  ; CHECK-NEXT: [[context:%.+]] = bitcast i8* %c to %struct.LLVMOpaqueContext*
  ; CHECK: %f = bitcast %struct.LLVMOpaqueValue* %{{.+}} to i8*
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (void ()* @f to i8*), i8* %c)
  ; CHECK-NEXT: %function = bitcast i8* %f to %struct.LLVMOpaqueValue*
  %function = bitcast i8* %f to %struct.LLVMOpaqueValue*
  call void @LLVMDumpValue(%struct.LLVMOpaqueValue* %function)
  call void @LLVMContextDispose(%struct.LLVMOpaqueContext* %context)
  ret void
}

%struct.LLVMOpaqueContext = type opaque
%struct.LLVMOpaqueValue = type opaque

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare %struct.LLVMOpaqueContext* @LLVMContextCreate()
declare void @LLVMContextDispose(%struct.LLVMOpaqueContext*)
declare %struct.LLVMOpaqueValue* @LLVMModuleCreateWithNameInContext(i8*, %struct.LLVMOpaqueContext*)
declare void @LLVMDumpValue(%struct.LLVMOpaqueValue*)
