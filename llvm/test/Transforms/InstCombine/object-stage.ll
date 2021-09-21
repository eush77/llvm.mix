; RUN: opt -instcombine -S < %s | FileCheck %s

; Check that a load from a pointer with object stage is not sunk.
; CHECK-LABEL: @sunk-load
define i32 @sunk-load(i1 %ann, i32* %p) {
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

; Check that object stage is not lost when used by a gep.
; CHECK-LABEL: @gep
define i32 @gep([2 x i32]* %p) {
  ; CHECK: call [2 x i32]* @llvm.object.stage.p0a2i32([2 x i32]* %p, i32 0)
  %p1 = call [2 x i32]* @llvm.object.stage.p0a2i32([2 x i32]* %p, i32 0)
  %p2 = getelementptr inbounds [2 x i32], [2 x i32]* %p1, i64 0, i64 0
  %x = load i32, i32* %p2
  ret i32 %x
}

declare i32* @llvm.object.stage.p0i32(i32*, i32)
declare [2 x i32]* @llvm.object.stage.p0a2i32([2 x i32]*, i32)
