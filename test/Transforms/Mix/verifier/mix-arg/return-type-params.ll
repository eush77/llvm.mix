; RUN: not opt -verify -disable-output <%s

declare void (i32)* @llvm.mix.0(i8*, metadata, ...)

define void @g(i32 %x) {
  %f = call void (i32)* (i8*, metadata, ...) @llvm.mix.0(i8* null, metadata !"g", i32 1)
  ret void
}
