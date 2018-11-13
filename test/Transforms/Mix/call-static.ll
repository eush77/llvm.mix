; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define i32 @f()
; CHECK-STAGE-LABEL: define i32 @f()
define i32 @f() stage(1) {
  ; CHECK-STAGE-NEXT: call i32 @g()
  %t = call i32 @g(i32 1)
  %t1 = add i32 %t, 1
  ; CHECK-STAGE-NEXT: ret i32 5
  ret i32 %t1
}

; CHECK-LABEL: define i32 @g(i32 %x)
; CHECK-STAGE-LABEL: define internal i32 @g()
define i32 @g(i32 %x) stage(1) {
  ; CHECK-STAGE-NEXT: call i32 @s()
  %t0 = call i32 @s(i32 %x, i32 %x)
  ; CHECK-STAGE-NEXT: call i32 @s.1()
  %t1 = call i32 @s(i32 %x, i32 %x)
  ; CHECK-STAGE-NEXT: ret i32 4
  %t2 = add i32 %t0, %t1
  ret i32 %t2
}

; CHECK-LABEL: define i32 @s(i32 %x, i32 %y)
; CHECK-STAGE-LABEL: define internal i32 @s()
define i32 @s(i32 %x, i32 %y) stage(1) {
  ; CHECK-STAGE-NEXT: ret i32 2
  %z = add i32 %x, %y
  ret i32 %z
}

; CHECK-STAGE-LABEL: define internal i32 @s.1()
; CHECK-STAGE-NEXT: ret i32 2

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call i8* @LLVMContextCreate()
  %function = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 ()* @f to i8*), i8* %context)
  %module = call i8* @LLVMGetGlobalParent(i8* %function)
  call void @LLVMDumpModule(i8* %module)
  call void @LLVMContextDispose(i8* %context)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
