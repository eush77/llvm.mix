; RUN: opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: warning: foo.c:5:5: in manyterm: Multiple stage(0) terminators of the entry block:
; CHECK: br label %left.exit, {{.*}}   ; stage(0), in %left
; CHECK: note: foo.c:6:6: in manyterm: Previous terminator is moved to stage(1):
; CHECK: br label %right.exit, {{.*}}  ; stage(0), in %right
; CHECK: warning: foo.c:10:10: in manyterm: Multiple stage(0) terminators of the entry block:
; CHECK: ret void, {{.*}}             ; stage(1), in %right.exit
; CHECK: note: foo.c:5:5: in manyterm: Previous terminator is moved to stage(1):
; CHECK: br label %left.exit, {{.*}}  ; stage(0), in %left
define void @manyterm(i1 stage(1) %x) stage(1) !dbg !3 {
  br i1 %x, label %left, label %right
left:
  br label %left.exit, !dbg !5
right:
  br label %right.exit, !dbg !6
left.exit:
  ret void
right.exit:
  ret void, !dbg !10
}

; CHECK: warning: foo.c:9:9: in manyterm1: Multiple stage(0) terminators of basic block %fork:
; CHECK: ret i32 1, {{.*}}      ; stage(0), in %merge
; CHECK: note: foo.c:7:7: in manyterm1: Previous terminator is moved to stage(1):
; CHECK: br label %id1, {{.*}}  ; stage(0), in %id
; CHECK: warning: foo.c:7:7: in manyterm1: Multiple stage(1) terminators of basic block %fork1:
; CHECK: br label %id1, {{.*}}    ; stage(0), in %id
; CHECK: note: foo.c:8:8: in manyterm1: Previous terminator is moved to stage(2):
; CHECK: br label %merge, {{.*}}  ; stage(1), in %merge1
; CHECK: warning: foo.c:9:9: in manyterm1: Multiple stage(1) terminators of basic block %fork:
; CHECK: ret i32 1, {{.*}}      ; stage(0), in %merge
; CHECK: note: foo.c:7:7: in manyterm1: Previous terminator is moved to stage(2):
; CHECK: br label %id1, {{.*}}  ; stage(1), in %id
define i32 @manyterm1(i1 stage(1) %x1, i1 stage(2) %x2) stage(2) !dbg !4 {
fork:
  br i1 %x1, label %fork1, label %merge
fork1:
  br i1 %x2, label %id, label %merge1
id:
  br label %id1, !dbg !7
id1:
  br label %merge1
merge1:
  br label %merge, !dbg !8
merge:
  ret i32 1, !dbg !9
}

!llvm.module.flags = !{!0}
!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !DIFile(filename: "foo.c", directory: "/")
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1)
!3 = distinct !DISubprogram(name: "manyterm", file: !1, scopeLine: 3, unit: !2)
!4 = distinct !DISubprogram(name: "manyterm1", file: !1, scopeLine: 4, unit: !2)
!5 = !DILocation(line: 5, column: 5, scope: !3)
!6 = !DILocation(line: 6, column: 6, scope: !3)
!7 = !DILocation(line: 7, column: 7, scope: !4)
!8 = !DILocation(line: 8, column: 8, scope: !4)
!9 = !DILocation(line: 9, column: 9, scope: !4)
!10 = !DILocation(line: 10, column: 10, scope: !3)
