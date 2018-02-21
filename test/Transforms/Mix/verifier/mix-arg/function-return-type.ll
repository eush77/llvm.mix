; RUN: not opt -verify -disable-output <%s

declare i32 ()* @llvm.mix.0(i8*, metadata, ...)

define void @g(i32 %x) {
  %f = call i32 ()* (i8*, metadata, ...) @llvm.mix.0(i8* null, metadata !"g", i32 1)
  ret void
}
