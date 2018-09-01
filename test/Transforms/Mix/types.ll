; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define i1 @i1()
; CHECK-STAGE-LABEL: define i1 @i1()
define i1 @i1() stage(1) {
  ; CHECK-STAGE: ret i1 false
  ret i1 false
}

; CHECK-LABEL: define void @i1.mix
define void @i1.mix(i8* %context) {
  ; CHECK: LLVMInt1TypeInContext
  %i1 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i1 ()* @i1 to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %i1)
  ret void
}

; CHECK-LABEL: define i44 @i44()
; CHECK-STAGE-LABEL: define i44 @i44()
define i44 @i44() stage(1) {
  ; CHECK-STAGE: ret i44 -8796093022208
  ret i44 8796093022208         ; 2^43
}

; CHECK-LABEL: define void @i44.mix
define void @i44.mix(i8* %context) {
  ; CHECK: LLVMIntTypeInContext
  %i44 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i44 ()* @i44 to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %i44)
  ret void
}

; CHECK-LABEL: define i256 @i256()
; CHECK-STAGE-LABEL: define i256 @i256()
define i256 @i256() stage(1) {
  ; CHECK-STAGE: ret i256 -57896044618658097711785492504343953926634992332820282019728792003956564819968
  ret i256 57896044618658097711785492504343953926634992332820282019728792003956564819968 ; 2^255
}

; CHECK-LABEL: define void @i256.mix
define void @i256.mix(i8* %context) {
  ; CHECK: LLVMIntTypeInContext
  %i256 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i256 ()* @i256 to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %i256)
  ret void
}

; CHECK-LABEL: define half @half()
; CHECK-STAGE-LABEL: define half @half()
define half @half() stage(1) {
  ; CHECK-STAGE: ret half 0xH0123
  ret half 0xH123
}

; CHECK-LABEL: define void @half.mix
define void @half.mix(i8* %context) {
  %half = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (half ()* @half to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %half)
  ret void
}

; CHECK-LABEL: define double @double()
; CHECK-STAGE-LABEL: define double @double()
define double @double() stage(1) {
  ; CHECK-STAGE: ret double 1.25
  ret double 1.25
}

; CHECK-LABEL: define void @double.mix
define void @double.mix(i8* %context) {
  %double = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (double ()* @double to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %double)
  ret void
}

; CHECK-LABEL: define fp128 @fp128()
; CHECK-STAGE-LABEL: define fp128 @fp128()
define fp128 @fp128() stage(1) {
  ; CHECK-STAGE: ret fp128 0xL{{0+}}AD
  ret fp128 0xL0AD
}

; CHECK-LABEL: define void @fp128.mix
define void @fp128.mix(i8* %context) {
  %fp128 = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (fp128 ()* @fp128 to i8*), i8* %context)
  call void @LLVMDumpValue(i8* %fp128)
  ret void
}

; CHECK-LABEL: define void @main()
define void @main() {
  %c = call i8* @LLVMContextCreate()
  call void @i1.mix(i8* %c)
  call void @i44.mix(i8* %c)
  call void @i256.mix(i8* %c)
  call void @half.mix(i8* %c)
  call void @double.mix(i8* %c)
  call void @fp128.mix(i8* %c)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
