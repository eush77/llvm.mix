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
; CHECK: [[module:%.+]] = call %struct.LLVMOpaqueModule* bitcast ({{.*}} @LLVMModuleCreateWithNameInContext to %struct.LLVMOpaqueModule* (i8*, %struct.LLVMOpaqueContext*)*)({{.*}}, %struct.LLVMOpaqueContext* [[context]])
; CHECK: ret %struct.LLVMOpaqueModule* [[module]]

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call %struct.LLVMContext* @LLVMContextCreate()
  %c = bitcast %struct.LLVMContext* %context to i8*
  ; CHECK: call %struct.LLVMOpaqueModule* @f.mix({{.*}}, i32 1)
  %m = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"f", i32 1)
  %module = bitcast i8* %m to %struct.Module*
  call void @LLVMDumpModule(%struct.Module* %module)
  call void @LLVMContextDispose(%struct.LLVMContext* %context)
  ret void
}

%struct.LLVMContext = type opaque
%struct.Module = type opaque

declare i8* @llvm.mix(i8*, metadata, ...)
declare %struct.LLVMContext* @LLVMContextCreate()
declare void @LLVMContextDispose(%struct.LLVMContext*)
declare %struct.Module* @LLVMModuleCreateWithNameInContext(i8*, %struct.LLVMContext*)
declare void @LLVMDumpModule(%struct.Module*)
