; RUN: opt -S -mix %s -o - | FileCheck %s --implicit-check-not=define
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define i1 @i1()
; CHECK-LABEL: define private %struct.LLVMOpaqueModule* @i1.mix
; CHECK-STAGE-LABEL: define i1 @i1()
define i1 @i1() {
  ; CHECK: LLVMInt1TypeInContext
  ; CHECK-STAGE: ret i1 false
  ret i1 false
}

; CHECK-LABEL: define i44 @i44()
; CHECK-LABEL: define private %struct.LLVMOpaqueModule* @i44.mix
; CHECK-STAGE-LABEL: define i44 @i44()
define i44 @i44() {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i44 -8796093022208
  ret i44 8796093022208         ; 2^43
}

; CHECK-LABEL: define i256 @i256()
; CHECK-LABEL: define private %struct.LLVMOpaqueModule* @i256.mix
; CHECK-STAGE-LABEL: define i256 @i256()
define i256 @i256() {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i256 -57896044618658097711785492504343953926634992332820282019728792003956564819968
  ret i256 57896044618658097711785492504343953926634992332820282019728792003956564819968 ; 2^255
}

; CHECK-LABEL: define half @half()
; CHECK-LABEL: define private %struct.LLVMOpaqueModule* @half.mix
; CHECK-STAGE-LABEL: define half @half()
define half @half() {
  ; CHECK-STAGE: ret half 0xH0123
  ret half 0xH123
}

; CHECK-LABEL: define double @double()
; CHECK-LABEL: define private %struct.LLVMOpaqueModule* @double.mix
; CHECK-STAGE-LABEL: define double @double()
define double @double() {
  ; CHECK-STAGE: ret double 1.25
  ret double 1.25
}

; CHECK-LABEL: define void @main()
define void @main() {
  %px = alloca i32
  %c = call i8* @LLVMContextCreate()
  %i1 = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"i1")
  call void @LLVMDumpModule(i8* %i1)
  %i44 = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"i44")
  call void @LLVMDumpModule(i8* %i44)
  %i256 = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"i256")
  call void @LLVMDumpModule(i8* %i256)
  %half = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"half")
  call void @LLVMDumpModule(i8* %half)
  %double = call i8* (i8*, metadata, ...) @llvm.mix(i8* %c, metadata !"double")
  call void @LLVMDumpModule(i8* %double)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, metadata, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
