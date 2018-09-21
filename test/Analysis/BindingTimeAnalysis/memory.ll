; RUN: opt -disable-output -print-bta %s 2>&1 | FileCheck %s --implicit-check-not="; stage"

declare i32* @llvm.object.stage.p0i32(i32*, i32)

; CHECK-LABEL: define {{.*}} @load
define i32 @load(i32 *%p) stage(1) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0) ; CHECK-NEXT: stage(0)
  %x = load i32, i32* %p1                            ; CHECK-NEXT: stage(0)
  ret i32 %x                                         ; CHECK-NEXT: stage(0)
}

; CHECK-LABEL: define {{.*}} @load1
define stage(1) i32 @load1(i32 *%p) stage(1) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 2) ; CHECK-NEXT: stage(0)
  %x = load i32, i32* %p1                            ; CHECK-NEXT: stage(1)
  ret i32 %x                                         ; CHECK-NEXT: stage(1)
}

; CHECK-LABEL: define {{.*}} @load2
define stage(1) i32 @load2(i32* stage(1) %p) stage(1) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 0) ; CHECK-NEXT: stage(1)
  %x = load i32, i32* %p1                            ; CHECK-NEXT: stage(1)
  ret i32 %x                                         ; CHECK-NEXT: stage(1)
}

; CHECK-LABEL: define {{.*}} @store
define void @store(i32* %p, i32 %x) stage(1) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 1) ; CHECK-NEXT: stage(0)
  store i32 %x, i32* %p1                            ; CHECK-NEXT: stage(0)
  ret void                                          ; CHECK-NEXT: stage(1)
}

; CHECK-LABEL: define {{.*}} @store1
define void @store1(i32* %p, i32 stage(1) %x) stage(2) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 2) ; CHECK-NEXT: stage(0)
  store i32 %x, i32* %p1                            ; CHECK-NEXT: stage(1)
  ret void                                          ; CHECK-NEXT: stage(2)
}

; CHECK-LABEL: define {{.*}} @store2
define void @store2(i32* %p, i32 stage(1) %x) stage(2) {
; CHECK-NEXT: stage(0)
  %p1 = call i32* @llvm.object.stage.p0i32(i32* %p, i32 1) ; CHECK-NEXT: stage(0)
  store i32 %x, i32* %p1                            ; CHECK-NEXT: stage(1)
  ret void                                          ; CHECK-NEXT: stage(2)
}
