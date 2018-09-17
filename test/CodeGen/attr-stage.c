// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

// CHECK: Function Attrs: {{.*}} stage(2)
// CHECK-NEXT: define stage(2) float @f(i32 stage(1) %a, float stage(2) %b) [[attr:#[0-9]+]]
// CHECK: {{^}}attributes [[attr]] = {{.*}} stage=2
__stage(2) float f(int __stage(1) a, float __stage(2) b) __stage(2) {
  return a + b;
}
