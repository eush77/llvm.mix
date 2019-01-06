// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

float f(int a, float b) __stage(1) {
  return a + b;
}

__attribute__((mix(f)))
void *f_gen(void *context, int a, float b);
// CHECK-LABEL: define i8* @f_gen(i8* %context, i32 %a, float %b)
// CHECK: [[val:%[^ ]+]] = call {{.*}} @llvm.mix(i8* bitcast (float (i32, float)* @f to i8*), i8* %context{{[^,]+}}, i32 %a{{[^,]+}}, float %b{{[^,]+}})
// CHECK-NEXT: ret i8* [[val]]
