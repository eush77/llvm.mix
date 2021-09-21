; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i32* @llvm.object.stage.p0i32(i32*, i32)

; CHECK: note: foo.c:4:4: in store: Given the declared object stage:
; CHECK: %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0), {{[^;]*}}
; CHECK: error: foo.c:5:5: in store: Storing to a stage(0) object:
; CHECK: store i32 %x, i32* %p1, {{[^;]*}}
define void @store(i32* %p, i32 %x) stage(1) !dbg !3 {
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0), !dbg !4
  store i32 %x, i32* %p1, !dbg !5
  ret void
}

!llvm.module.flags = !{!0}
!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !DIFile(filename: "foo.c", directory: "/")
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1)
!3 = distinct !DISubprogram(name: "store", file: !1, scopeLine: 3, unit: !2)
!4 = !DILocation(line: 4, column: 4, scope: !3)
!5 = !DILocation(line: 5, column: 5, scope: !3)
