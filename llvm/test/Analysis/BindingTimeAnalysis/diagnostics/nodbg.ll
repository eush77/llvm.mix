; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i32* @llvm.object.stage.p0i32(i32*, i32)

; CHECK: note: in f: Given the declared object stage:
; CHECK: %p1 = tail call i32* @llvm.object.stage.p0i32(i32* %p, i32 0)
; CHECK: error: in f: Storing to a stage(0) object:
; CHECK: store i32 1, i32* %p1
define void @f(i32 *%p) stage(1) {
  %p1 = tail call i32* @llvm.object.stage.p0i32(i32* %p, i32 0)
  store i32 1, i32* %p1
  ret void
}
