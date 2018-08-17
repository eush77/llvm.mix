// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

int f(int a, int b) {
  return a + b;
}

__attribute__((mix_ir(f)))
void *gen_f_ir(void*, int, int);
// CHECK-LABEL: define i8* @gen_f_ir
// CHECK: unreachable
