; RUN: opt -S -mix %s -o - \
; RUN: | FileCheck %s --check-prefix=STAGE0 --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0-LABEL: define stage(1) i32 @f(i32 %n, i32 stage(1) %x)
; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* %context, i32 %n)
; STAGE0: [[param_types:%.+]] = alloca %struct.LLVMOpaqueType*
; STAGE0: [[param_ptr:%.+]] = getelementptr %struct.LLVMOpaqueType*, %struct.LLVMOpaqueType** [[param_types]], i32 0
; STAGE0: [[i32:%.+]] = call %struct.LLVMOpaqueType* @LLVMInt32TypeInContext
; STAGE0: store %struct.LLVMOpaqueType* [[i32]], %struct.LLVMOpaqueType** [[param_ptr]]
; STAGE0: [[function_type:%.+]] = call %struct.LLVMOpaqueType* @LLVMFunctionType(%struct.LLVMOpaqueType* [[i32]], %struct.LLVMOpaqueType** [[param_types]], i32 1, i32 0)

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.mix(i8** %mix.context, i32 %n)
; STAGE1-LABEL: define dso_local i32 @f(i32 %x)
define stage(1) i32 @f(i32 %n, i32 stage(1) %x) stage(1) {
  ; STAGE1: %y = add i32 %x, 4
  %y = add i32 %x, %n
  ret i32 %y
}

; STAGE0-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; STAGE0: call %struct.LLVMOpaqueValue* @f.main({{.*}}, i32 4)
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32, i32)* @f to i8*), i8* %c, i32 4)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
