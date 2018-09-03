; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define i32 @f(i32 %x)
define i32 @f(i32 %x) {
  %y = add i32 %x, 1
  ret i32 %y
}

; CHECK-LABEL: define stage(1) i32 @g(i32 %x)
; CHECK-STAGE-LABEL: define i32 @g()
define stage(1) i32 @g(i32 %x) stage(1) {
  ; CHECK-STAGE-NEXT: %y = call i32 inttoptr (i64 {{[0-9]+}} to i32 (i32)*)(i32 4)
  %y = call i32 @f(i32 %x)
  ; CHECK-STAGE-NEXT: ret i32 %y
  ret i32 %y
}

; CHECK-LABEL: define void @main()
define void @main() {
  %context = call i8* @LLVMContextCreate()
  %function = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 (i32)* @g to i8*), i8* %context, i32 4)
  call void @LLVMDumpValue(i8* %function)
  call void @LLVMContextDispose(i8* %context)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
