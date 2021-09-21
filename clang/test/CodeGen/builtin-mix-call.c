// RUN: %clang_cc1 -emit-llvm %s -O1 -o - | FileCheck %s

int f(int x) __stage(1) {
  return x;
}

// CHECK-LABEL: @g
__stage(1) void *g(int x) __stage(1) {
  // CHECK: call i8* (i8*, ...) @llvm.mix.call(i8* bitcast (i32 (i32)* @f to i8*), i32 %x)
  return __builtin_mix_call(f, x);
}
