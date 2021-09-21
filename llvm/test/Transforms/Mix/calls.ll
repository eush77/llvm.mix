; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE1-LABEL: declare i32 @e(i32)
define i32 @e(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; STAGE1-LABEL: define dso_local i32 @g()
define stage(1) i32 @g(i32 %x) stage(1) {
  ; STAGE1-NEXT: %t0 = call i32 @e(i32 4)
  %t0 = call i32 @e(i32 %x)
  ; STAGE1-NEXT: %t1 = call i32 inttoptr (i64 {{[0-9]+}} to i32 (i32)*)(i32 %t0)
  %t1 = call i32 @i(i32 %t0)
  ; STAGE1: %t2 = call i32 @f(i32 %t1)
  %t2 = call i32 @f(i32 %t1, i32 4)
  ; STAGE1: %t3 = call i32 @f.1(i32 %t2)
  %t3 = call i32 @f(i32 %t2, i32 6)
  ; STAGE1: ret i32 %t3
  ret i32 %t3
}

define internal i32 @i(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; STAGE1-LABEL: define internal i32 @f(i32 %x)
define stage(1) i32 @f(i32 stage(1) %x, i32 %n) stage(1) {
  ; STAGE1-NEXT: %y = add i32 %x, 4
  %y = add i32 %x, %n
  %m = add i32 %n, 1
  ; STAGE1-NEXT: %z = call i32 @h(i32 %y)
  %z = call i32 @h(i32 %y, i32 %m)
  ; STAGE1-NEXT: ret i32 %z
  ret i32 %z
}

; STAGE1-LABEL: define internal i32 @h(i32 %x)
define stage(1) i32 @h(i32 stage(1) %x, i32 %m) stage(1) {
  ; STAGE1-NEXT: %y = mul i32 %x, 5
  %y = mul i32 %x, %m
; STAGE1-NEXT: ret i32 %y
  ret i32 %y
}

; STAGE1-LABEL: define internal i32 @f.1(i32 %x)
; STAGE1-NEXT: %y = add i32 %x, 6
; STAGE1-NEXT: %z = call i32 @h.2(i32 %y)
; STAGE1-NEXT: ret i32 %z

; STAGE1-LABEL: define internal i32 @h.2(i32 %x)
; STAGE1-NEXT: %y = mul i32 %x, 7
; STAGE1-NEXT: ret i32 %y

define void @main() {
  %context = call i8* @LLVMContextCreate()
  %function = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i32)* @g to i8*), i8* %context, i32 4)
  %module = call i8* @LLVMGetGlobalParent(i8* %function)
  call void @LLVMDumpModule(i8* %module)
  call void @LLVMContextDispose(i8* %context)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
