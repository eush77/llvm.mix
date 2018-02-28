; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE

; CHECK-LABEL: define void @f(i32 %x)
; CHECK-STAGE-LABEL: define void @f()
define void @f(i32 %x) {
  ; CHECK-NEXT: ret void
  ; CHECK-STAGE-NEXT: ret void
  ret void
}

; CHECK: define private i8* @f.mix(i8* [[context:%.+]], i32 %x)
; CHECK: [[module:%.+]] = call i8* bitcast ({{.*}} @LLVMModuleCreateWithNameInContext to i8* (i8*, i8*)*)({{.*}}, i8* [[context]])
; CHECK: ret i8* [[module]]

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call %struct.LLVMContext* @LLVMContextCreate()
  %c = bitcast %struct.LLVMContext* %context to i8*
  ; CHECK: %m = call i8* @f.mix(i8* %c, i32 1)
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
