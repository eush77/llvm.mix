; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: note: foo.c:3:0: in retstage: Given the declared return stage(0)
; CHECK: error: foo.c:4:4: in retstage: Inferred return stage(1) contradicts the declared return stage:
; CHECK: ret i32 %x, {{.*}}  ; stage(1), in %entry
define i32 @retstage(i32 stage(1) %x) stage(1) !dbg !3 {
entry:
  ret i32 %x, !dbg !4
}

!llvm.module.flags = !{!0}
!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !DIFile(filename: "foo.c", directory: "/")
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1)
!3 = distinct !DISubprogram(name: "retstage", file: !1, scopeLine: 3, unit: !2)
!4 = !DILocation(line: 4, column: 4, scope: !3)
