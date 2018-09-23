; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

define void @f(i32 %y) stage(1) !dbg !3 { ret void }

; CHECK: note: foo.c:3:0: in f: Given the declared parameter i32 %y of stage(0)
; CHECK: note: foo.c:4:0: in argstage: Given the argument i32 %x of stage(1)
; CHECK: error: foo.c:5:5: in argstage: Inferred argument stage contradicts the declared parameter stage:
; CHECK: call void @f(i32 %x), {{.*}}  ; in %entry
define void @argstage(i32 stage(1) %x) stage(1) !dbg !4 {
entry:
  call void @f(i32 %x), !dbg !5
  ret void
}

!llvm.module.flags = !{!0}
!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !DIFile(filename: "foo.c", directory: "/")
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1)
!3 = distinct !DISubprogram(name: "f", file: !1, scopeLine: 3, unit: !2)
!4 = distinct !DISubprogram(name: "argstage", file: !1, scopeLine: 4, unit: !2)
!5 = !DILocation(line: 5, column: 5, scope: !4)
