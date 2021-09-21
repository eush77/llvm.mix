; RUN: opt -S -mix %s -o - | FileCheck %s --check-prefix=STAGE0

; NOTE: Using O1 here because the interpreter does not support constants of
; type `half'.
;
; RUN: opt -S -mix -O1 %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | FileCheck %s --implicit-check-not=define -check-prefix=STAGE1
; RUN: opt -S -mix -O1 %s -o - | lli -force-interpreter - 2>&1 \
; RUN: | opt -verify -disable-output

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @i1.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local i1 @i1()
define i1 @i1() stage(1) {
  ; STAGE0: LLVMInt1TypeInContext
  ; STAGE1: ret i1 false
  ret i1 false
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @i44.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local i44 @i44()
define i44 @i44() stage(1) {
  ; STAGE0: LLVMIntTypeInContext
  ; STAGE1: ret i44 -8796093022208
  ret i44 8796093022208         ; 2^43
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @i256.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local i256 @i256()
define i256 @i256() stage(1) {
  ; STAGE0: LLVMIntTypeInContext
  ; STAGE1: ret i256 -57896044618658097711785492504343953926634992332820282019728792003956564819968
  ret i256 57896044618658097711785492504343953926634992332820282019728792003956564819968 ; 2^255
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @half.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local half @half()
define half @half() stage(1) {
  ; STAGE0: LLVMHalfTypeInContext
  ; STAGE1: ret half 0xH0123
  ret half 0xH123
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @double.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local double @double()
define double @double() stage(1) {
  ; STAGE0: LLVMDoubleTypeInContext
  ; STAGE1: ret double 1.25
  ret double 1.25
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @fp128.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local fp128 @fp128()
define fp128 @fp128() stage(1) {
  ; STAGE0: LLVMFP128TypeInContext
  ; STAGE1: ret fp128 0xL{{0+}}AD
  ret fp128 0xL0AD
}

%opaque = type opaque

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @struct.opaque.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: source_filename = "struct.opaque.module"
define %opaque* @struct.opaque() stage(1) {
  ; STAGE0: LLVMStructCreateNamed
  ; STAGE0-NOT: LLVMStructSetBody
  ; STAGE1: %opaque = type opaque
  ; STAGE1: define dso_local %opaque* @struct.opaque()
  ; STAGE1: ret %opaque* null
  ret %opaque* null
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @struct.empty.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local {} @struct.empty()
define {} @struct.empty() stage(1) {
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE0: LLVMStructTypeInContext
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE1: ret {} zeroinitializer
  ret {} {}
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @struct.literal.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local { i32, i32 } @struct.literal()
define { i32, i32 } @struct.literal() stage(1) {
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE0: LLVMStructTypeInContext
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE1: ret { i32, i32 } { i32 1, i32 2 }
  ret { i32, i32 } { i32 1, i32 2 }
}

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @struct.packed.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local <{ i32, i32 }> @struct.packed()
define <{ i32, i32 }> @struct.packed() stage(1) {
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE0: LLVMStructTypeInContext
  ; STAGE0-NOT: LLVMStructCreateNamed
  ; STAGE1: ret <{ i32, i32 }> <{ i32 1, i32 2 }>
  ret <{ i32, i32 }> <{ i32 1, i32 2 }>
}

%struct = type { i32, i32 }

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @struct.named.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: source_filename = "struct.named.module"
define %struct @struct.named() stage(1) {
  ; STAGE0: LLVMStructCreateNamed
  ; STAGE0: LLVMStructSetBody
  ; STAGE1: %struct = type  { i32, i32 }
  ; STAGE1: define dso_local %struct @struct.named()
  ; STAGE1: ret %struct { i32 1, i32 2 }
  ret %struct { i32 1, i32 2 }
}

declare i32 @llvm.read_register.i32(metadata)

; STAGE0-LABEL: define private %struct.LLVMOpaqueValue* @metadata.main(%struct.LLVMOpaqueContext* %context)
; STAGE1-LABEL: define dso_local i32 @metadata()
define stage(1) i32 @metadata() stage(1) {
  ; STAGE0: LLVMMetadataTypeInContext
  ; STAGE1: call i32 @llvm.read_register.i32(metadata !0)
  %x = tail call i32 @llvm.read_register.i32(metadata !0)
  ret i32 %x
}

; STAGE1: !0 = !{!"sp\00"}
!0 = !{!"sp\00"}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %i1 = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i1 ()* @i1 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i1)
  %i44 = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i44 ()* @i44 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i44)
  %i256 = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i256 ()* @i256 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %i256)
  %half = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (half ()* @half to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %half)
  %double = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (double ()* @double to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %double)
  %fp128 = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (fp128 ()* @fp128 to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %fp128)
  %struct.opaque = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (%opaque* ()* @struct.opaque to i8*), i8* %c)
  %struct.opaque.m = call i8* @LLVMGetGlobalParent(i8* %struct.opaque)
  call void @LLVMDumpModule(i8* %struct.opaque.m)
  %struct.empty = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast ({} ()* @struct.empty to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.empty)
  %struct.literal = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast ({ i32, i32 } ()* @struct.literal to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.literal)
  %struct.packed = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (<{ i32, i32 }> ()* @struct.packed to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %struct.packed)
  %struct.named = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (%struct ()* @struct.named to i8*), i8* %c)
  %struct.named.m = call i8* @LLVMGetGlobalParent(i8* %struct.named)
  call void @LLVMDumpModule(i8* %struct.named.m)
  %metadata = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 ()* @metadata to i8*), i8* %c)
  %metadata.m = call i8* @LLVMGetGlobalParent(i8* %metadata)
  call void @LLVMDumpModule(i8* %metadata.m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare void @LLVMDumpValue(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
