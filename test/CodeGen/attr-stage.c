// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

// CHECK: define float @f(i32 stage(1) %a, float stage(2) %b)
float f(int __stage(1) a, float __stage(2) b) {
  return a + b;
}
