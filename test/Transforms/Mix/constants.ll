; RUN: opt -S -mix %s | FileCheck %s --enable-var-scope --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; Check that staged constants defined in a non-dominator do not get reused
; STAGE0-LABEL: define {{.*}} @f.mix
; STAGE1-LABEL: define {{.*}} @f
define i32 @f(i1 %b) stage(1) {
  br i1 %b, label %left, label %right
  ; STAGE1: ret i32 4

; STAGE0-LABEL: {{^}}right:
right:
  ; STAGE0: [[val:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt
  ; STAGE0-NEXT: call %struct.LLVMOpaqueValue* @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* [[val]])
  ret i32 4

; STAGE0-LABEL: {{^}}left:
left:
  ; STAGE0: [[val:%.+]] = call %struct.LLVMOpaqueValue* @LLVMConstInt
  ; STAGE0-NEXT: call %struct.LLVMOpaqueValue* @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* [[val]])
  ret i32 4
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1)* @f to i8*), i8* %c, i1 0)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
