; RUN: opt -S -mix -globaldce %s \
; RUN: | FileCheck %s --check-prefix=STAGE0 --implicit-check-not=define
; RUN: opt -S -mix %s | lli -force-interpreter 2>&1 \
; RUN: | FileCheck %s --check-prefix=STAGE1
; RUN: opt -S -mix %s | lli -force-interpreter | opt -verify -disable-output

; @g must be removed after Mix because it cannot be code-generated due to the
; use of llvm.mix.call

; STAGE0-LABEL: define {{.*}} @g.main
; STAGE0-LABEL: define {{.*}} @g.mix
define internal stage(1) i32 @g(i32 %x, i1 stage(1) %b) stage(1) {
  br i1 %b, label %left, label %right

left:
  ; STAGE0: %p.left = call {{.*}} @f.mix
  %p.left = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (i32 (i32)* @f to i8*), i32 %x)
  br label %exit

right:
  %x1 = add i32 %x, 1
  ; STAGE0: %p.right = call {{.*}} @f.mix
  %p.right = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (i32 (i32)* @f to i8*), i32 %x1)
  br label %exit

; STAGE1-LABEL: {{^}}exit:
exit:
  ; STAGE1-NEXT: phi i8* [ bitcast (i32 ()* [[f_left:@f.*]] to i8*), %left ], [ bitcast (i32 ()* [[f_right:@f.*]] to i8*), %right ]
  %p = phi i8* [ %p.left, %left ], [ %p.right, %right ]
  ; STAGE1-NEXT: %f = bitcast i8* %p to i32 ()*
  %f = bitcast i8* %p to i32 ()*
  ; STAGE1-NEXT: call i32 %f()
  %y = call i32 %f()
  ret i32 %y
}

; STAGE0-LABEL: define {{.*}} @f
; STAGE0-LABEL: define {{.*}} @f.mix
; STAGE1: define internal i32 [[f_left]]
; STAGE1-NEXT: ret i32 4
; STAGE1: define internal i32 [[f_right]]
; STAGE1-NEXT: ret i32 5
define i32 @f(i32 %x) stage(1) {
  ret i32 %x
}

; STAGE0-LABEL: define {{.*}} @main
define void @main() {
  %c = call i8* @LLVMContextCreate()
  %g = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32 (i32, i1)* @g to i8*), i8* %c, i32 4)
  %m = call i8* @LLVMGetGlobalParent(i8* %g)
  call void @LLVMDumpModule(i8* %m)
  call void @LLVMContextDispose(i8* %c)
  ret void
}

declare i8* @llvm.mix(i8*, i8*, ...)
declare i8* @llvm.mix.call(i8*, ...)
declare i8* @LLVMContextCreate()
declare void @LLVMContextDispose(i8*)
declare void @LLVMDumpModule(i8*)
declare i8* @LLVMGetGlobalParent(i8*)
