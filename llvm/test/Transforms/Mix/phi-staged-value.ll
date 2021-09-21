; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: define {{.*}} @static.mix
; STAGE1-LABEL: define {{.*}} @static
define i32 @static(i1 %p) stage(1) {
  br i1 %p, label %one, label %zero

zero:
  br label %exit

one:
  br label %exit

; STAGE0-LABEL: {{^}}exit:
; STAGE1-LABEL: {{^}}exit:
exit:
  ; STAGE0-NEXT: %phi = phi i32 [ 0, %zero ], [ 1, %one ]
  ; STAGE1-NEXT: ret i32 1
  %phi = phi i32 [ 0, %zero ], [ 1, %one ]
  ret i32 %phi
}

; STAGE0-LABEL: define {{.*}} @dynamic.mix
; STAGE1-LABEL: define {{.*}} @dynamic
define stage(1) i32 @dynamic(i1 %p, i32 stage(1) %v0, i32 stage(1) %v1) stage(1) {
  br i1 %p, label %one, label %zero

zero:
  br label %exit

one:
  br label %exit

; STAGE0-LABEL: {{^}}exit:
; STAGE1-LABEL: {{^}}exit:
exit:
  ; STAGE0-NEXT: %phi = phi %struct.LLVMOpaqueValue* [ %v0, %zero ], [ %v1, %one ]
  ; STAGE0: call %struct.LLVMOpaqueValue* @LLVMBuildRet({{.*}}, %struct.LLVMOpaqueValue* %phi)
  ; STAGE1-NEXT: ret i32 %v1
  %phi = phi i32 [ %v0, %zero ], [ %v1, %one ]
  ret i32 %phi
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %static = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1)* @static to i8*), i8* %c, i1 1)
  call void @LLVMDumpValue(i8* %static)
  %dynamic = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1, i32, i32)* @dynamic to i8*), i8* %c, i1 1)
  call void @LLVMDumpValue(i8* %dynamic)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
