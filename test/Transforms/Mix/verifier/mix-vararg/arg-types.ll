; RUN: not opt -verify -disable-output <%s

declare i8* @llvm.mix(i8*, metadata, ...)

define void @g(i32 %x, ...) {
  %f = call i8* (i8*, metadata, ...) @llvm.mix(i8* null, metadata !"g", i1 1, i1 1)
  ret void
}
