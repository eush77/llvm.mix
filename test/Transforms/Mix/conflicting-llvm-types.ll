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

; CHECK: define private %struct.LLVMOpaqueValue* @f.mix(%struct.LLVMOpaqueContext* [[context:%.+]], i32 %x)
; CHECK: [[module:%.+]] = call %struct.LLVMOpaqueModule* bitcast ({{.*}} @LLVMModuleCreateWithNameInContext to %struct.LLVMOpaqueModule* (i8*, %struct.LLVMOpaqueContext*)*)({{.*}}, %struct.LLVMOpaqueContext* [[context]])
; CHECK: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction(%struct.LLVMOpaqueModule* [[module]],
; CHECK: ret %struct.LLVMOpaqueValue* [[function]]

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call %struct.LLVMContext* @LLVMContextCreate()
  %c = bitcast %struct.LLVMContext* %context to i8*
  ; CHECK: call %struct.LLVMOpaqueValue* @f.mix({{.*}}, i32 1)
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (void (i32)* @f to i8*), i8* %c, i32 1)
  %function = bitcast i8* %f to %struct.Value*
  call void @LLVMDumpValue(%struct.Value* %function)
  call void @LLVMContextDispose(%struct.LLVMContext* %context)
  ret void
}

%struct.LLVMContext = type opaque
%struct.Value = type opaque

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare %struct.LLVMContext* @LLVMContextCreate()
declare void @LLVMContextDispose(%struct.LLVMContext*)
declare %struct.Value* @LLVMModuleCreateWithNameInContext(i8*, %struct.LLVMContext*)
declare void @LLVMDumpValue(%struct.Value*)
