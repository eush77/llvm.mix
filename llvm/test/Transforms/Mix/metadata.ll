; RUN: opt -S -enable-new-pm=0 -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: define {{.*}} @instruction_metadata.mix
; STAGE1-LABEL: define {{.*}} @instruction_metadata
define void @instruction_metadata(i32 %n, i32 stage(1) %x) stage(1) {
  ; STAGE0: %m = add i32 %n, 1, !name !0, !void !1
  %m = add i32 %n, 1, !name !0, !void !1
  ; STAGE0: @LLVMSetMetadata
  ; STAGE1: %y = add i32 %x, 5, !name [[m2:![0-9]]], !void [[m1:![0-9]]]
  %y = add i32 %x, %m, !name !2, !void !1
  ret void
}

; Check that staged metadata defined in a non-dominator do not get reused
;
; STAGE0-LABEL: define {{.*}} @non_domination.mix
; STAGE1-LABEL: define {{.*}} @non_domination
define void @non_domination(i32 %n, i32 stage(1) %x) stage(1) {
  %b = icmp eq i32 %n, 1
  br i1 %b, label %left, label %right
  ; STAGE1: ret void, !meta [[m0:![0-9]]]

; STAGE0: {{^}}right:
right:
  ; STAGE0: call %struct.LLVMOpaqueValue* @LLVMMDStringInContext
  ; STAGE0: call void @LLVMSetMetadata
  ret void, !meta !0

; STAGE0: {{^}}left:
left:
  ; STAGE0: call %struct.LLVMOpaqueValue* @LLVMMDStringInContext
  ; STAGE0: call void @LLVMSetMetadata
  ret void, !meta !0
}

; STAGE0: !0 = !{!"m"}
; STAGE1-DAG: [[m0]] = !{!"m"}
!0 = !{!"m"}

; STAGE0: !1 = !{}
; STAGE1-DAG: [[m1]] = !{}
!1 = !{}

; STAGE0: !2 = !{!"y"}
; STAGE1-DAG: [[m2]] = !{!"y"}
!2 = !{!"y"}

define void @f(i32 %n, i32 stage(1) %x) stage(1) {
  call void @instruction_metadata(i32 %n, i32 %x)
  call void @non_domination(i32 %n, i32 %x)
  ret void
}

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
