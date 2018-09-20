// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s

// CHECK: Function Attrs: {{.*}} stage(2)
// CHECK-NEXT: define stage(2) float @f(i32 stage(1) %a, float stage(2) %b) [[attr:#[0-9]+]]
__stage(2) float f(int __stage(1) a, float __stage(2) b) __stage(2) {
  return a + b;
}

// CHECK-LABEL: define i32* @g
int *g(struct { int x __stage(3); } * s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: call void @llvm.object.stage.p0i32(i32* [[gep]], i32 3)
  return &s->x;
}

struct node {
  struct node *x __stage(1);
};

// CHECK-LABEL: define %struct.node* @h(%struct.node* %s)
struct node *h(struct node *s) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: call void @llvm.object.stage.p0p0s_struct.nodes(%struct.node** [[gep]], i32 1)
  // CHECK-NEXT: load %struct.node*, %struct.node** [[gep]]
  return s->x;
}

// CHECK: {{^}}attributes [[attr]] = {{.*}} stage=2
