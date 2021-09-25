; XFAIL: *

; RUN: opt -S -enable-new-pm=0 -mix %s | FileCheck %s --check-prefix=STAGE0
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -enable-new-pm=0 -mix %s | lli -force-interpreter | opt -verify -disable-output

; STAGE0-LABEL: define {{.*}} @f.mix
; STAGE1-LABEL: define {{.*}} @f
define void @f(i32 %n, i32 stage(1) %x) stage(1) {
  ; STAGE0: %m = add i32 %n, 1, !name !0, !void !1
  %m = add i32 %n, 1, !name !0, !void !1
  ; STAGE0: @LLVMSetMetadata
  ; STAGE1: %y = add i32 %x, 5, !name !0, !void !1
  %y = add i32 %x, %m, !name !2, !void !1
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %x.addr, metadata !10, metadata !DIExpression()), !dbg !11
  call void @non_domination(i32 %n, i32 %x)
  ret void
}

; Check that staged metadata defined in a non-dominator do not get reused
;
; STAGE0-LABEL: define {{.*}} @non_domination.mix
; STAGE1-LABEL: define {{.*}} @non_domination
define void @non_domination(i32 %n, i32 stage(1) %x) stage(1) {
  %b = icmp eq i32 %n, 1
  br i1 %b, label %left, label %right
  ; STAGE1: ret void, !meta !2

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

!0 = !{!"m"}                    ; STAGE0: !0 = !{!"m"}
!1 = !{}                        ; STAGE0: !1 = !{}
!2 = !{!"y"}                    ; STAGE1: !0 = !{!"y"}
; STAGE1: !1 = !{}

!llvm.module.flags = !{!3}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !DIFile(filename: "metadata.ll", directory: "llvm/test/Transforms/Mix")
!5 = distinct !DICompileUnit(language: DW_LANG_C99, file: !4, producer: "clang", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!6 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!7 = !{null, !6, !6}
!8 = !DISubroutineType(types: !7)
!9 = distinct !DISubprogram(name: "f", scope: !4, file: !4, line: 1, type: !8, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !5, retainedNodes: !1)
!10 = !DILocalVariable(name: "x", arg: 2, scope: !9, file: !4, line: 1, type: !6)
!11 = !DILocation(line: 1, column: 37, scope: !9)

declare void @llvm.dbg.declare(metadata, metadata, metadata)

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
