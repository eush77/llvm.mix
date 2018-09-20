; RUN: llc < %s

declare i32* @llvm.object.stage.p0i32(i32*, i32)

define i32* @f(i32* %p) {
  %x = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0)
  ret i32* %x
}
