; RUN: opt -S -enable-new-pm=0 -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: define {{.*}} @instruction_metadata.mix
; STAGE1-LABEL: define {{.*}} @instruction_metadata
define void @instruction_metadata(i32 %n, i32 stage(1) %x) stage(1) {
  ; STAGE0: %m = add i32 %n, 1
  ; STAGE0-NOT: !meta
  %m = add i32 %n, 1, !meta !0
  ; STAGE0-NOT: @LLVMSetMetadata
  ; STAGE1: %y = add i32 %x, 5
  ; STAGE1-NOT: !meta
  %y = add i32 %x, %m, !meta !0
  ret void
}

!0 = !{}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, i32)* @instruction_metadata to i8*), i8* %c, i32 4)
  %m = call i8* @LLVMGetGlobalParent(i8* %f)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
