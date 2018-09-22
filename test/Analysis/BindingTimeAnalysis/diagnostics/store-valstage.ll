; RUN: not opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i32* @llvm.object.stage.p0i32(i32*, i32)

; CHECK: error: Inferred value stage(1) contradicts the object stage(1) at store:
; CHECK: store i32 %x, i32* %p1  ; in entry block
define void @store(i32* %p, i32 stage(1) %x) stage(1) {
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 1)
  store i32 %x, i32* %p1
  ret void
}
