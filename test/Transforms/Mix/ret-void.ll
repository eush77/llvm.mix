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
; CHECK: [[module:%.+]] = call i8* @LLVMModuleCreateWithNameInContext({{.*}}, i8* [[context]])
; CHECK: [[function:%.+]] = call i8* @LLVMAddFunction(i8* [[module]],
; CHECK: [[entry:%.+]] = call i8* @LLVMAppendBasicBlockInContext(i8* [[context]], i8* [[function]],
; CHECK: [[builder:%.+]] = call i8* @LLVMCreateBuilderInContext(i8* [[context]])
; CHECK: call void @LLVMPositionBuilderAtEnd(i8* [[builder]], i8* [[entry]])
; CHECK: call i8* @LLVMBuildRetVoid(i8* [[builder]])
; CHECK: call void @LLVMDisposeBuilder(i8* [[builder]])
; CHECK: ret i8* [[module]]

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; CHECK: %m = call i8* @f.mix(i8* %c, i32 1)
  %m = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"f", i32 1)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, metadata, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
