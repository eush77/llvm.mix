; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define stage(1) i32 @g(i32 %x)
; CHECK-STAGE-LABEL: define i32 @g()
define stage(1) i32 @g(i32 %x) stage(1) {
  ; CHECK-STAGE-NEXT: %t0 = call i32 @e(i32 4)
  %t0 = call i32 @e(i32 %x)
  ; CHECK-STAGE-NEXT: %t1 = call i32 inttoptr (i64 {{[0-9]+}} to i32 (i32)*)(i32 %t0)
  %t1 = call i32 @i(i32 %t0)
  ; CHECK-STAGE: %t2 = call i32 @f(i32 %t1)
  %t2 = call i32 @f(i32 %t1, i32 4)
  ; CHECK-STAGE: %t3 = call i32 @f.1(i32 %t2)
  %t3 = call i32 @f(i32 %t2, i32 6)
  ; CHECK-STAGE: ret i32 %t3
  ret i32 %t3
}

; CHECK-LABEL: define i32 @e(i32 %x)
; CHECK-STAGE-LABEL: declare i32 @e(i32)
define i32 @e(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; CHECK-LABEL: define internal i32 @i(i32 %x)
define internal i32 @i(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; CHECK-LABEL: define stage(1) i32 @f(i32 stage(1) %x, i32 %n)
; CHECK-STAGE-LABEL: define internal i32 @f(i32 %x)
define stage(1) i32 @f(i32 stage(1) %x, i32 %n) stage(1) {
  ; CHECK-STAGE-NEXT: %y = add i32 %x, 4
  %y = add i32 %x, %n
  %m = add i32 %n, 1
  ; CHECK-STAGE-NEXT: %z = call i32 @h(i32 %y)
  %z = call i32 @h(i32 %y, i32 %m)
  ; CHECK-STAGE-NEXT: ret i32 %z
  ret i32 %z
}

; CHECK-LABEL: define stage(1) i32 @h(i32 stage(1) %x, i32 %m)
; CHECK-STAGE-LABEL: define internal i32 @h(i32 %x)
define stage(1) i32 @h(i32 stage(1) %x, i32 %m) stage(1) {
  ; CHECK-STAGE-NEXT: %y = mul i32 %x, 5
  %y = mul i32 %x, %m
; CHECK-STAGE-NEXT: ret i32 %y
  ret i32 %y
}

; CHECK-STAGE-LABEL: define internal i32 @f.1(i32 %x)
; CHECK-STAGE-NEXT: %y = add i32 %x, 6
; CHECK-STAGE-NEXT: %z = call i32 @h.2(i32 %y)
; CHECK-STAGE-NEXT: ret i32 %z

; CHECK-STAGE-LABEL: define internal i32 @h.2(i32 %x)
; CHECK-STAGE-NEXT: %y = mul i32 %x, 7
; CHECK-STAGE-NEXT: ret i32 %y

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call i8* @LLVMContextCreate()
  %function = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32)* @g to i8*), i8* %context, i32 4)
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
