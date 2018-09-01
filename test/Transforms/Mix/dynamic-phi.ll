; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define stage(1) i32 @f()
; CHECK-STAGE-LABEL: define i32 @f()
define stage(1) i32 @f() stage(1) {
; CHECK-STAGE-LABEL: entry:
entry:
  ; CHECK-STAGE-NEXT: alloca
  %box = alloca i32
  ; CHECK-STAGE-NEXT: store
  store i32 5, i32* %box
  ; CHECK-STAGE-NEXT: load
  %v.0 = load i32, i32* %box
  ; CHECK-STAGE-NEXT: br {{.*}} %loop
  br label %loop

; CHECK-STAGE-LABEL: loop:
loop:
  ; CHECK-STAGE-NEXT: phi {{.*}} %entry {{.*}} %latch
  %v.1 = phi i32 [ %v.0, %entry ], [ %v.2, %latch ]
  ; CHECK-STAGE-NEXT: trunc
  %odd = trunc i32 %v.1 to i1
  ; CHECK-STAGE-NEXT: br {{.*}} %increase, {{.*}} %decrease
  br i1 %odd, label %increase, label %decrease

; CHECK-STAGE-LABEL: increase:
increase:
  ; CHECK-STAGE-NEXT: mul
  %triple = mul i32 %v.1, 3
  ; CHECK-STAGE-NEXT: add
  %triplep1 = add i32 %triple, 1
  ; CHECK-STAGE-NEXT: br {{.*}} %latch
  br label %latch

; CHECK-STAGE-LABEL: decrease:
decrease:
  ; CHECK-STAGE-NEXT: udiv
  %half = udiv i32 %v.1, 2
  ; CHECK-STAGE-NEXT: br {{.*}} %latch
  br label %latch

; CHECK-STAGE-LABEL: latch:
latch:
  ; CHECK-STAGE-NEXT: phi {{.*}} %increase {{.*}} %decrease
  %v.2 = phi i32 [ %triplep1, %increase ], [ %half, %decrease ]
  ; CHECK-STAGE-NEXT: icmp eq
  %end = icmp eq i32 %v.2, 1
  ; CHECK-STAGE-NEXT: br {{.*}} %exit, {{.*}} %loop
  br i1 %end, label %exit, label %loop

; CHECK-STAGE-LABEL: exit:
exit:
  ; CHECK-STAGE-NEXT: ret
  ret i32 %v.2
}

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  ; CHECK: [[context:%.+]] = bitcast i8* %c to %struct.LLVMOpaqueContext*
  ; CHECK: [[module:%.+]] = call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithNameInContext({{.*}}, %struct.LLVMOpaqueContext* [[context]])
  ; CHECK: [[function:%.+]] = call %struct.LLVMOpaqueValue* @LLVMAddFunction(%struct.LLVMOpaqueModule* [[module]],
  ; CHECK: %f = bitcast %struct.LLVMOpaqueValue* [[function]] to i8*
  %f = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 ()* @f to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %f)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
