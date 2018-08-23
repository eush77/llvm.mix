; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define i1 @i1()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i1.mix
; CHECK-STAGE-LABEL: define i1 @i1()
define i1 @i1() {
  ; CHECK: LLVMInt1TypeInContext
  ; CHECK-STAGE: ret i1 false
  ret i1 false
}

; CHECK-LABEL: define i44 @i44()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i44.mix
; CHECK-STAGE-LABEL: define i44 @i44()
define i44 @i44() {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i44 -8796093022208
  ret i44 8796093022208         ; 2^43
}

; CHECK-LABEL: define i256 @i256()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i256.mix
; CHECK-STAGE-LABEL: define i256 @i256()
define i256 @i256() {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i256 -57896044618658097711785492504343953926634992332820282019728792003956564819968
  ret i256 57896044618658097711785492504343953926634992332820282019728792003956564819968 ; 2^255
}

; CHECK-LABEL: define half @half()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @half.mix
; CHECK-STAGE-LABEL: define half @half()
define half @half() {
  ; CHECK-STAGE: ret half 0xH0123
  ret half 0xH123
}

; CHECK-LABEL: define double @double()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @double.mix
; CHECK-STAGE-LABEL: define double @double()
define double @double() {
  ; CHECK-STAGE: ret double 1.25
  ret double 1.25
}

; CHECK-LABEL: define fp128 @fp128()
; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @fp128.mix
; CHECK-STAGE-LABEL: define fp128 @fp128()
define fp128 @fp128() {
  ; CHECK-STAGE: ret fp128 0xL{{0+}}AD
  ret fp128 0xL0AD
}

; CHECK-LABEL: define void @main()
define void @main() {
  %px = alloca i32
  %c = call i8* @LLVMContextCreate()
  %i1 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i1 ()* @i1 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i1)
  %i44 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i44 ()* @i44 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i44)
  %i256 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i256 ()* @i256 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i256)
  %half = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (half ()* @half to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %half)
  %double = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (double ()* @double to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %double)
  %fp128 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (fp128 ()* @fp128 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %fp128)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
