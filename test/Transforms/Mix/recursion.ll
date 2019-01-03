; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1 \
; RUN:                --implicit-check-not='{{define|call|ret}}'
; RUN: opt -S -mix %s | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0-LABEL: define private { i32, %struct.LLVMOpaqueValue* } @f.mix(i8** %mix.context, i32 %n)
; STAGE1: define dso_local i32 [[f4:@f]]()
; STAGE1: call i32 [[f3:@f.+]]()
; STAGE1: ret i32 16
define i32 @f(i32 %n) stage(1) {
  ; STAGE0: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction
  %p = icmp eq i32 %n, 0
  br i1 %p, label %exit, label %rec

rec:                            ; STAGE0-LABEL: {{^}}rec:
  ; STAGE0: [[n1:%.+]] = sub i32 %n, 1
  %n1 = sub i32 %n, 1
  ; STAGE0: [[struct:%.+]] = call { i32, %struct.LLVMOpaqueValue* } @f.mix(i8** %mix.context, i32 [[n1]])
  ; STAGE0-DAG: [[static:%.+]] = extractvalue { i32, %struct.LLVMOpaqueValue* } [[struct]], 0
  ; STAGE0-DAG: [[residue:%.+]] = extractvalue { i32, %struct.LLVMOpaqueValue* } [[struct]], 1
  ; STAGE0-DAG: @LLVMBuildCall({{.*}}, %struct.LLVMOpaqueValue* [[residue]], {{.*}})
  %x = call i32 @f(i32 %n1)
  ; STAGE0: [[mul:%.+]] = mul i32 [[static]], 2
  %x1 = mul i32 %x, 2
  ; STAGE0: [[zext:%.+]] = zext i32 [[mul]] to i64
  ; STAGE0: [[lift:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 [[zext]], i32 0)
  ; STAGE0: @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* [[lift]])
  ; STAGE0: [[mrv1:%.+]] = insertvalue { i32, %struct.LLVMOpaqueValue* } undef, i32 [[mul]], 0
  ; STAGE0-NEXT: [[mrv2:%.+]] = insertvalue { i32, %struct.LLVMOpaqueValue* } [[mrv1]], %struct.LLVMOpaqueValue* [[function]], 1
  ; STAGE0-NEXT: ret { i32, %struct.LLVMOpaqueValue* } [[mrv2]]
  ret i32 %x1

exit:                           ; STAGE0-LABEL: {{^}}exit:
  ; STAGE0: [[base:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt({{.*}}, i64 1, i32 0)
  ; STAGE0: @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* [[base]])
  ; STAGE0: [[mrv2:%.+]] = insertvalue { i32, %struct.LLVMOpaqueValue* } { i32 1, %struct.LLVMOpaqueValue* undef }, %struct.LLVMOpaqueValue* [[function]], 1
  ; STAGE0-NEXT: ret { i32, %struct.LLVMOpaqueValue* } [[mrv2]]
  ret i32 1
}

; STAGE1: define internal i32 [[f3]]
; STAGE1: call i32 [[f2:@f.+]]()
; STAGE1: ret i32 8

; STAGE1: define internal i32 [[f2]]
; STAGE1: call i32 [[f1:@f.+]]()
; STAGE1: ret i32 4

; STAGE1: define internal i32 [[f1]]
; STAGE1: call i32 [[f0:@f.+]]()
; STAGE1: ret i32 2

; STAGE1: define internal i32 [[f0]]
; STAGE1: ret i32 1

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32)* @f to i8*), i8* %c, i32 4)
  %m = call i8* @LLVMGetGlobalParent(i8* %f)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
declare void @LLVMDumpModule(i8*)
