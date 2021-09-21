; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: @static.mix(i8** %mix.context, i32 %x)
; STAGE1-LABEL: @static()
define i32 @static(i32 %x) stage(1) {
  ; STAGE0: switch i32 %x, label %minus-one
  ; STAGE1: br label %one
  switch i32 %x, label %minus-one [
    i32 0, label %one           ; STAGE0-NEXT: i32 0, label %one
    i32 1, label %one           ; STAGE0-NEXT: i32 1, label %one
    i32 2, label %zero          ; STAGE0-NEXT: i32 2, label %zero
  ]

; STAGE0: {{^}}minus-one:
; STAGE1-NOT: {{^}}minus-one:
minus-one:
  ret i32 -1

; STAGE0: {{^}}zero:
; STAGE1-NOT: {{^}}zero:
zero:
  ret i32 0

; STAGE0: {{^}}one:
; STAGE1: {{^}}one:
one:
  ret i32 1
}

; STAGE0-LABEL: @dynamic.mix(i8** %mix.context)
; STAGE1-LABEL: @dynamic(i32 %x)
define stage(1) i32 @dynamic(i32 stage(1) %x) stage(1) {
  ; STAGE0: LLVMBuildSwitch
  ; STAGE0: LLVMAddCase
  ; STAGE0: LLVMAddCase
  ; STAGE0: LLVMAddCase
  ; STAGE1: switch i32 %x, label %minus-one
  switch i32 %x, label %minus-one [
    i32 0, label %one           ; STAGE1-NEXT: i32 0, label %one
    i32 1, label %one           ; STAGE1-NEXT: i32 1, label %one
    i32 2, label %zero          ; STAGE1-NEXT: i32 2, label %zero
  ]

; STAGE0-NOT: {{^}}minus-one:
; STAGE1: {{^}}minus-one:
minus-one:
  ret i32 -1

; STAGE0-NOT: {{^}}zero:
; STAGE1-DAG: {{^}}zero:
zero:
  ret i32 0

; STAGE0-NOT: {{^}}one:
; STAGE1-DAG: {{^}}one:
one:
  ret i32 1
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %static = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i32)* @static to i8*), i8* %c, i32 1)
  call void @LLVMDumpValue(i8* %static)
  %dynamic = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i32)* @dynamic to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %dynamic)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
