; RUN: opt -verify -disable-output <%s

declare void ()* @llvm.mix.0(i8*, metadata, ...)

define void @g(i32 %x, ...) {
  %f = call void ()* (i8*, metadata, ...) @llvm.mix.0(i8* null, metadata !"g", i32 1)
  ret void
}
