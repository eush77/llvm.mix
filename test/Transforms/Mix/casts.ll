; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0: %t0 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t1 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t2 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t3 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t4 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t5 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t6 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t7 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t8 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %t9 = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %tA = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %tB = call %struct.LLVMOpaqueValue* @LLVMBuildCast
; STAGE0: %tC = call %struct.LLVMOpaqueValue* @LLVMBuildCast
define void @casts(i64 stage(1) %x) stage(1) {
  %t0 = trunc i64 %x to i32         ; STAGE1: trunc i64 %x to i32
  %t1 = zext i32 %t0 to i64         ; STAGE1: zext i32 %t0 to i64
  %t2 = sext i32 %t0 to i64         ; STAGE1: sext i32 %t0 to i64
  %t3 = bitcast i64 %x to double    ; STAGE1: bitcast i64 %x to double
  %t4 = fptoui double %t3 to i32    ; STAGE1: fptoui double %t3 to i32
  %t5 = fptosi double %t3 to i32    ; STAGE1: fptosi double %t3 to i32
  %t6 = uitofp i32 %t4 to double    ; STAGE1: uitofp i32 %t4 to double
  %t7 = sitofp i32 %t5 to double    ; STAGE1: sitofp i32 %t5 to double
  %t8 = fptrunc double %t3 to float ; STAGE1: fptrunc double %t3 to float
  %t9 = fpext float %t8 to double   ; STAGE1: fpext float %t8 to double
  %tA = inttoptr i32 %t0 to i8*     ; STAGE1: inttoptr i32 %t0 to i8*
  %tB = ptrtoint i8* %tA to i32     ; STAGE1: ptrtoint i8* %tA to i32
  %tC = addrspacecast i8* %tA to i8 addrspace(1)* ; STAGE1: addrspacecast i8* %tA to i8 addrspace(1)*
  ret void
}

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %casts = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i64)* @casts to i8*), i8* %c)
  call void @LLVMDumpValue(i8* %casts)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpValue(i8*)
