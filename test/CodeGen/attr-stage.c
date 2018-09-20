// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s -implicit-check-not=object.stage

// CHECK: Function Attrs: {{.*}} stage(2)
// CHECK-NEXT: define stage(2) float @paramstage(i32 stage(1) %a, float stage(2) %b) [[attr:#[0-9]+]]
__stage(2) float paramstage(int __stage(1) a, float __stage(2) b) __stage(2) {
  return a + b;
}

// CHECK-LABEL: define i32* @fieldstage
int *fieldstage(struct { int x __stage(3); } * s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: call void @llvm.object.stage.p0i32(i32* [[gep]], i32 3)
  return &s->x;
}

// CHECK: declare void @llvm.object.stage.p0i32

struct node {
  struct node *x __stage(1);
};

// CHECK-LABEL: define %struct.node* @fieldstage1(%struct.node* %s)
struct node *fieldstage1(struct node *s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: call void @llvm.object.stage.p0p0s_struct.nodes(%struct.node** [[gep]], i32 1)
  // CHECK-NEXT: load %struct.node*, %struct.node** [[gep]]
  return s->x;
}

// CHECK: declare void @llvm.object.stage.p0p0s_struct.nodes

// CHECK-LABEL: define %struct.node* @fieldstage2(%struct.node* %s)
struct node *fieldstage2(struct node *s) {
  return s->x;
}

// CHECK: {{^}}attributes [[attr]] = {{.*}} stage=2
