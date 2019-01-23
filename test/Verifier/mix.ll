; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i8* @llvm.mix(i8*, i8*, ...)

define void @f(i32) stage(1) { ret void }

define i8* @f.mix(i8* %ctx, i32 %x) {
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32)* @f to i8*), i8* %ctx, i32 %x)
  ret i8* %f
}

define i8* @constexpr0(i8* %x) {
  ; CHECK: First argument of llvm.mix is not a constant expression
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* %x, i8* null)
  ret i8* %f
}

define i8* @constexpr1() {
  ; CHECK: First argument of llvm.mix is not a constant expression
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* null, i8* null)
  ret i8* %f
}

define i8* @notid() {
  ; CHECK: First argument of llvm.mix is not a function identifier
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (i32* inttoptr (i32 4 to i32*) to i8*), i8* null)
  ret i8* %f
}

declare void @g() stage(1)

define i8* @nobody() {
  ; CHECK: Function @g is not defined in this module
  ; CHECK: %g = call {{.*}}
  %g = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void ()* @g to i8*), i8* null)
  ret i8* %g
}

define void @l() { ret void }

define i8* @notstaged() {
  ; CHECK: Function @l is not staged
  ; CHECK: %l = call {{.*}}
  %l = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void ()* @l to i8*), i8* null)
  ret i8* %l
}

define i8* @argcount0() {
  ; CHECK: Not enough arguments for @f
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32)* @f to i8*), i8* null)
  ret i8* %f
}

define i8* @argcount1() {
  ; CHECK: Too many arguments for @f
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32)* @f to i8*), i8* null, i32 1, i32 1)
  ret i8* %f
}

define void @h(i32, ...) stage(1) { ret void }

define i8* @argcount2() {
  ; CHECK: Not enough arguments for @h
  ; CHECK: %h = call {{.*}}
  %h = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, ...)* @h to i8*), i8* null)
  ret i8* %h
}

define i8* @argcount3() {
  %h = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, ...)* @h to i8*), i8* null, i32 1, i32 1)
  ret i8* %h
}

define void @k(i32 stage(1), float, i8* stage(2)) stage(2) { ret void }

define i8* @argcount4() {
  %k = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, float, i8*)* @k to i8*), i8* null, float 1.)
  ret i8* %k
}

define i8* @argcount5() {
  ; CHECK: Too many arguments for @k
  ; CHECK: %k = call {{.*}}
  %k = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, float, i8*)* @k to i8*), i8* null, i32 1, float 1., i8* null)
  ret i8* %k
}

define void @j(i32, i32 %a) stage(1) { ret void }

define i8* @argtypes0() {
  ; CHECK: The type of argument 2 does not match the type of parameter 0 of @j
  ; CHECK: %j = call {{.*}}
  %j = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, i32)* @j to i8*), i8* null, i16 1, i32 1)
  ret i8* %j
}

define i8* @argtypes1() {
  ; CHECK: The type of argument 3 does not match the type of parameter %a of @j
  ; CHECK: %j = call {{.*}}
  %j = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, i32)* @j to i8*), i8* null, i32 1, i16 1)
  ret i8* %j
}

define i8* @argtypes2() {
  ; CHECK: The type of argument 2 does not match the type of parameter 1 of @k
  ; CHECK: %k = call {{.*}}
  %k = call i8* (i8*, i8*, ...) @llvm.mix(i8* bitcast (void (i32, float, i8*)* @k to i8*), i8* null, i32 1)
  ret i8* %k
}

; CHECK: {{.*}} error: input module is broken!
