// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

float f(int a, float b) __stage(1) {
  return a + b;
}

typedef struct LLVMOpaqueContext *LLVMContextRef;
typedef struct LLVMOpaqueValue *LLVMValueRef;

__attribute__((mix(f)))
LLVMValueRef f_gen(LLVMContextRef context, int a, float b);
// CHECK-LABEL: define %struct.LLVMOpaqueValue* @f_gen(%struct.LLVMOpaqueContext* %context, i32 %a, float %b)
// CHECK: [[context:%.+]] = bitcast %struct.LLVMOpaqueContext* %context{{[^ ]*}} to i8*
// CHECK-NEXT: [[val:%.+]] = call {{.*}} @llvm.mix(i8* bitcast (float (i32, float)* @f to i8*), i8* [[context]], i32 %a{{[^,]*}}, float %b{{[^,]*}})
// CHECK-NEXT: [[val1:%.+]] = bitcast i8* [[val]] to %struct.LLVMOpaqueValue*
// CHECK-NEXT: ret %struct.LLVMOpaqueValue* [[val1]]
