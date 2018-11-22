; RUN: opt -S -mix %s -o - | FileCheck %s

; NOTE: Using O1 here because the interpreter does not support constants of
; type `half'.
;
; RUN: opt -S -mix -O1 %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=CHECK-STAGE
; RUN: opt -S -mix -O1 %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i1.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define i1 @i1()
define i1 @i1() stage(1) {
  ; CHECK: LLVMInt1TypeInContext
  ; CHECK-STAGE: ret i1 false
  ret i1 false
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i44.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define i44 @i44()
define i44 @i44() stage(1) {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i44 -8796093022208
  ret i44 8796093022208         ; 2^43
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @i256.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define i256 @i256()
define i256 @i256() stage(1) {
  ; CHECK: LLVMIntTypeInContext
  ; CHECK-STAGE: ret i256 -57896044618658097711785492504343953926634992332820282019728792003956564819968
  ret i256 57896044618658097711785492504343953926634992332820282019728792003956564819968 ; 2^255
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @half.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define half @half()
define half @half() stage(1) {
  ; CHECK: LLVMHalfTypeInContext
  ; CHECK-STAGE: ret half 0xH0123
  ret half 0xH123
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @double.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define double @double()
define double @double() stage(1) {
  ; CHECK: LLVMDoubleTypeInContext
  ; CHECK-STAGE: ret double 1.25
  ret double 1.25
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @fp128.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define fp128 @fp128()
define fp128 @fp128() stage(1) {
  ; CHECK: LLVMFP128TypeInContext
  ; CHECK-STAGE: ret fp128 0xL{{0+}}AD
  ret fp128 0xL0AD
}

%opaque = type opaque

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @struct.opaque.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: source_filename = "struct.opaque.module"
define %opaque* @struct.opaque() stage(1) {
  ; CHECK: LLVMStructCreateNamed
  ; CHECK-NOT: LLVMStructSetBody
  ; CHECK-STAGE: %opaque = type opaque
  ; CHECK-STAGE: define %opaque* @struct.opaque()
  ; CHECK-STAGE: ret %opaque* null
  ret %opaque* null
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @struct.empty.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define {} @struct.empty()
define {} @struct.empty() stage(1) {
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK: LLVMStructTypeInContext
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK-STAGE: ret {} zeroinitializer
  ret {} {}
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @struct.literal.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define { i32, i32 } @struct.literal()
define { i32, i32 } @struct.literal() stage(1) {
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK: LLVMStructTypeInContext
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK-STAGE: ret { i32, i32 } { i32 1, i32 2 }
  ret { i32, i32 } { i32 1, i32 2 }
}

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @struct.packed.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define <{ i32, i32 }> @struct.packed()
define <{ i32, i32 }> @struct.packed() stage(1) {
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK: LLVMStructTypeInContext
  ; CHECK-NOT: LLVMStructCreateNamed
  ; CHECK-STAGE: ret <{ i32, i32 }> <{ i32 1, i32 2 }>
  ret <{ i32, i32 }> <{ i32 1, i32 2 }>
}

%struct = type { i32, i32 }

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @struct.named.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: source_filename = "struct.named.module"
define %struct @struct.named() stage(1) {
  ; CHECK: LLVMStructCreateNamed
  ; CHECK: LLVMStructSetBody
  ; CHECK-STAGE: %struct = type  { i32, i32 }
  ; CHECK-STAGE: define %struct @struct.named()
  ; CHECK-STAGE: ret %struct { i32 1, i32 2 }
  ret %struct { i32 1, i32 2 }
}

declare i32 @llvm.read_register.i32(metadata)

; CHECK-LABEL: define private %struct.LLVMOpaqueValue* @metadata.main(%struct.LLVMOpaqueContext* %context)
; CHECK-STAGE-LABEL: define i32 @metadata()
define stage(1) i32 @metadata() stage(1) {
  ; CHECK: LLVMMetadataTypeInContext
  ; CHECK-STAGE: call i32 @llvm.read_register.i32(metadata !0)
  %x = tail call i32 @llvm.read_register.i32(metadata !0)
  ret i32 %x
}

; CHECK-STAGE: !0 = !{!"sp\00"}
!0 = !{!"sp\00"}

define void @main() {
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
  %struct.opaque = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (%opaque* ()* @struct.opaque to i8*), i8* %c)
  %struct.opaque.m = call i8* @LLVMGetGlobalParent(i8* %struct.opaque)
  call void @LLVMDumpModule(i8* %struct.opaque.m)
  %struct.empty = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast ({} ()* @struct.empty to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.empty)
  %struct.literal = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast ({ i32, i32 } ()* @struct.literal to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.literal)
  %struct.packed = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (<{ i32, i32 }> ()* @struct.packed to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.packed)
  %struct.named = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (%struct ()* @struct.named to i8*), i8* %c)
  %struct.named.m = call i8* @LLVMGetGlobalParent(i8* %struct.named)
  call void @LLVMDumpModule(i8* %struct.named.m)
  %metadata = call i8* (i8*, i8*, ...) @llvm.mix.ir(i8* bitcast (i32 ()* @metadata to i8*), i8* %c)
  %metadata.m = call i8* @LLVMGetGlobalParent(i8* %metadata)
  call void @LLVMDumpModule(i8* %metadata.m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix.ir(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare void @LLVMDumpValue(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
