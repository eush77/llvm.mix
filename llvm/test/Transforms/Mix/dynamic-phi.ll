; RUN: opt -S -mix %s -o - \
; RUN: | FileCheck %s --check-prefix=STAGE0 --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0-LABEL: define stage(1) i32 @f()
; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.main(%struct.LLVMOpaqueContext* %context)
; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @f.mix(i8** %mix.context)
; STAGE1-LABEL: define dso_local i32 @f()
define stage(1) i32 @f() stage(1) {
; STAGE1-LABEL: entry:
entry:
  ; STAGE1-NEXT: alloca
  %box = alloca i32
  ; STAGE1-NEXT: store
  store i32 5, i32* %box
  ; STAGE1-NEXT: load
  %v.0 = load i32, i32* %box
  ; STAGE1-NEXT: br {{.*}} %loop
  br label %loop

; STAGE1-LABEL: loop:
loop:
  ; STAGE1-NEXT: phi {{.*}} %entry {{.*}} %latch
  %v.1 = phi i32 [ %v.0, %entry ], [ %v.2, %latch ]
  ; STAGE1-NEXT: trunc
  %odd = trunc i32 %v.1 to i1
  ; STAGE1-NEXT: br {{.*}} %increase, {{.*}} %decrease
  br i1 %odd, label %increase, label %decrease

; STAGE1-LABEL: increase:
increase:
  ; STAGE1-NEXT: mul
  %triple = mul i32 %v.1, 3
  ; STAGE1-NEXT: add
  %triplep1 = add i32 %triple, 1
  ; STAGE1-NEXT: br {{.*}} %latch
  br label %latch

; STAGE1-LABEL: decrease:
decrease:
  ; STAGE1-NEXT: udiv
  %half = udiv i32 %v.1, 2
  ; STAGE1-NEXT: br {{.*}} %latch
  br label %latch

; STAGE1-LABEL: latch:
latch:
  ; STAGE1-NEXT: phi {{.*}} %increase {{.*}} %decrease
  %v.2 = phi i32 [ %triplep1, %increase ], [ %half, %decrease ]
  ; STAGE1-NEXT: icmp eq
  %end = icmp eq i32 %v.2, 1
  ; STAGE1-NEXT: br {{.*}} %exit, {{.*}} %loop
  br i1 %end, label %exit, label %loop

; STAGE1-LABEL: exit:
exit:
  ; STAGE1-NEXT: ret
  ret i32 %v.2
}

; STAGE0-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; STAGE0: call %struct.LLVMOpaqueValue* @f.main
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 ()* @f to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
