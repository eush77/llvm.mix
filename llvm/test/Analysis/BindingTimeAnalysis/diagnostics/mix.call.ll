; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: note: foo.c:3:0: in dynamic-arg: Given the argument i32 %x of stage(1)
; CHECK: error: foo.c:4:4: in dynamic-arg: Inferred argument stage is invalid for the call to llvm.mix.call:
; CHECK: %f = call i8* (i8*, ...) @llvm.mix.call({{.*}}  ; in %entry
define stage(1) i8* @dynamic-arg(i32 stage(1) %x) stage(1) !dbg !3 {
entry:
  %f = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32)* @f to i8*), i32 %x), !dbg !4
  ret i8* %f
}

define void @f(i32) stage(1) { ret void }

declare i8* @llvm.mix.call(i8*, ...)

!llvm.module.flags = !{!0}
!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !DIFile(filename: "foo.c", directory: "/")
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1)
!3 = distinct !DISubprogram(name: "dynamic-arg", file: !1, scopeLine: 3, unit: !2)
!4 = !DILocation(line: 4, column: 4, scope: !3)
