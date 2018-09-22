; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i32* @llvm.object.stage.p0i32(i32*, i32)

; CHECK: error: Storing to a stage(0) object:
; CHECK: %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0) ; in entry block
; CHECK: store i32 %x, i32* %p1                               ; in entry block
define void @store(i32* %p, i32 %x) stage(1) {
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0)
  store i32 %x, i32* %p1
  ret void
}
