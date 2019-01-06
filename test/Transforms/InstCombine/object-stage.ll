; RUN: opt -instcombine -S < %s | FileCheck %s

; Check that a load from a pointer with object stage is not sunk.
define i32 @test(i1 %ann, i32* %p) {
  br i1 %ann, label %annotated, label %unannotated

annotated:
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0)
  %x = load i32, i32* %p1
  br label %exit

unannotated:
  %p2 = getelementptr i32, i32* %p, i32 1
  %y = load i32, i32* %p2
  br label %exit

; CHECK: {{^}}exit:
; CHECK-NEXT: %z = phi i32
; CHECK-NEXT: ret i32 %z
exit:                           ;
  %z = phi i32 [ %x, %annotated ], [ %y, %unannotated]
  ret i32 %z
}

declare i32* @llvm.object.stage.p0i32(i32*, i32)
