; RUN: not opt -verify -disable-output <%s

declare i8* @llvm.mix(i8*, metadata, ...)

declare void @h(i32 %x)

define void @g(i32 %x) {
  %m = call i8* (i8*, metadata, ...) @llvm.mix(i8* null, metadata !"h", i32 1)
  ret void
}
