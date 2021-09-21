; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE1-LABEL: define {{.*}} @indirect
define stage(1) i32 @indirect(i1 %p, i32 stage(1) %x) stage(1) {
zero:
  br i1 %p, label %exit, label %cond

cond:
  br i1 %p, label %one, label %meet

one:
  br label %meet

meet:
  %v0 = phi i32 [ 1, %one ], [ %x, %cond ]
  br label %exit

exit:                           ; STAGE1-LABEL: {{^}}exit:
  %v1 = phi i32 [ 0, %zero ], [ %v0, %meet ]
  ret i32 %v1                   ; STAGE1-NEXT: ret i32 %x
}

; STAGE1-LABEL: define {{.*}} @loop
define stage(1) i32 @loop(i1 %p, i32 stage(1) %x) stage(1) {
entry:
  br label %header

header:
  %v = phi i32 [ 0, %entry ], [ %v, %header ]
  br i1 %p, label %header, label %exit

exit:                           ; STAGE1-LABEL: {{^}}exit:
  ret i32 %v                    ; STAGE1-NEXT: ret i32 0
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %indirect = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1, i32)* @indirect to i8*), i8* %c, i1 0)
  call void @LLVMDumpValue(i8* %indirect)
  %loop = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i1, i32)* @loop to i8*), i8* %c, i1 0)
  call void @LLVMDumpValue(i8* %loop)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
