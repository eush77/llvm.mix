; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define stage(1) i32 @f(i32 %n, i32 stage(1) %x)
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* %context, i32 %n)
; CHECK: [[param_types:%.+]] = alloca %struct.LLVMOpaqueType*
; CHECK: [[param_ptr:%.+]] = getelementptr %struct.LLVMOpaqueType*, %struct.LLVMOpaqueType** [[param_types]], i32 0
; CHECK: [[i32:%.+]] = call %struct.LLVMOpaqueType* @LLVMInt32TypeInContext
; CHECK: store %struct.LLVMOpaqueType* [[i32]], %struct.LLVMOpaqueType** [[param_ptr]]
; CHECK: [[function_type:%.+]] = call %struct.LLVMOpaqueType* @LLVMFunctionType(%struct.LLVMOpaqueType* [[i32]], %struct.LLVMOpaqueType** [[param_types]], i32 1, i32 0)

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @f.mix(i8** %mix.context, i32 %n)
; CHECK-STAGE-LABEL: define i32 @f(i32 %x)
define stage(1) i32 @f(i32 %n, i32 stage(1) %x) stage(1) {
  ; CHECK-STAGE: %y = add i32 %x, 4
  %y = add i32 %x, %n
  ret i32 %y
}

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; CHECK: call %struct.LLVMOpaqueValue* @f.main({{.*}}, i32 4)
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32, i32)* @f to i8*), i8* %c, i32 4)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
