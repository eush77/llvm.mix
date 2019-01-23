; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i8* @llvm.mix.call(i8*, ...)

define void @f(i32) stage(1) { ret void }

define i8* @f.mix(i32 %x) stage(1) {
  %f = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32)* @f to i8*), i32 %x)
  ret i8* %f
}

define i8* @nonstaged(i32 %x) {
  ; CHECK: llvm.mix.call used in a non-staged function
  ; CHECK: %f = call {{.*}}
  %f = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32)* @f to i8*), i32 %x)
  ret i8* %f
}

define void @j(i32, i32 %a) stage(1) { ret void }

define i8* @argtypes() stage(1) {
  ; CHECK: The type of argument 1 does not match the type of parameter 0 of @j
  ; CHECK: %j = call {{.*}}
  %j = call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (void (i32, i32)* @j to i8*), i16 1, i32 1)
  ret i8* %j
}

; CHECK: {{.*}} error: input module is broken!
