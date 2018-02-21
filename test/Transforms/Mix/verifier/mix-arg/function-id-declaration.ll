; RUN: not opt -verify -disable-output <%s

declare void ()* @llvm.mix.0(i8*, metadata, ...)

declare void @h(i32 %x)

define void @g(i32 %x) {
  %f = call void ()* (i8*, metadata, ...) @llvm.mix.0(i8* null, metadata !"h", i32 1)
  ret void
}
