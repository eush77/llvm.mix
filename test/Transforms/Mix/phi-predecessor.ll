; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; Static PHINode must use static predecessors
;
; STAGE0-LABEL: define {{.*}} @f.mix
define void @f(i1 %p, i1 stage(1) %b) stage(1) {
entry:                          ; stage(0)
  br i1 %p, label %exit, label %preheader

preheader:                      ; stage(0)
  br i1 %b, label %tail, label %header

header:                         ; stage(1)
  br i1 %b, label %header, label %tail

tail:                           ; stage(1)
  br label %exit

; STAGE0: {{^}}exit:
exit:                           ; stage(0)
  ; STAGE0-NEXT: %phi = phi i32 [ 0, %entry ], [ 1, %preheader ]
  %phi = phi i32 [ 0, %entry ], [ 1, %tail ]
  ret void
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i1, i1)* @f to i8*), i8* %c, i1 1)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
