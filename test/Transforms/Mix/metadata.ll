; RUN: opt -S -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

define void @f(i32 %n, i32 stage(1) %x) stage(1) {
  ; STAGE0: %m = add i32 %n, 1, !name !0, !void !1
  %m = add i32 %n, 1, !name !0, !void !1
  ; STAGE0: @LLVMSetMetadata
  ; STAGE1: %y = add i32 %x, 5, !name !0, !void !1
  %y = add i32 %x, %m, !name !2, !void !1
  ; STAGE0: @LLVMValueAsMetadata
  ; STAGE0: @LLVMMetadataAsValue
  ; STAGE1: %z = call {{.*}} metadata i32 %y
  %z = call {i8*, i1} @llvm.type.checked.load(i8* null, i32 %y, metadata i32 %y)
  ; STAGE0: @LLVMValueAsMetadata
  ; STAGE0: @LLVMMetadataAsValue
  ; STAGE1: %a = call {{.*}} metadata i32 8
  %a = call {i8*, i1} @llvm.type.checked.load(i8* null, i32 %y, metadata i32 8)
  ret void
}

!0 = !{!"m"}                    ; STAGE0: !0 = !{!"m"}
!1 = !{}                        ; STAGE0: !1 = !{}
!2 = !{!"y"}                    ; STAGE1: !0 = !{!"y"}
; STAGE1: !1 = !{}

declare {i8*, i1} @llvm.type.checked.load(i8*, i32, metadata)

define void @main() {
  %c = call i8* @LLVMContextCreate()
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, i32)* @f to i8*), i8* %c, i32 4)
  %m = call i8* @LLVMGetGlobalParent(i8* %f)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
