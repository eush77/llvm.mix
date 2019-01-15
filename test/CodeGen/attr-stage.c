// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s -enable-var-scope -implicit-check-not=object.stage

// CHECK: Function Attrs: {{.*}} stage(2)
// CHECK-NEXT: define stage(2) float @paramstage(i32 stage(1) %a, float stage(2) %b) [[$attr:#[0-9]+]]
__stage(2) float paramstage(int __stage(1) a, float __stage(2) b) __stage(2) {
  return a + b;
}

// CHECK-LABEL: define i32* @fieldstage
int *fieldstage(struct { int x __stage(3); } * s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: [[x:%.+]] = call i32* @llvm.object.stage.p0i32(i32* [[gep]], i32 3)
  // CHECK-NEXT: ret i32* [[x]]
  return &s->x;
}

// CHECK: declare i32* @llvm.object.stage.p0i32

struct node {
  struct node *x __stage(1);
};

// CHECK-LABEL: define %struct.node* @fieldstage1(%struct.node* %s)
struct node *fieldstage1(struct node *s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: [[x:%.+]] = call %struct.node** @llvm.object.stage.p0p0s_struct.nodes(%struct.node** [[gep]], i32 1)
  // CHECK-NEXT: load %struct.node*, %struct.node** [[x]]
  return s->x;
}

// CHECK: declare %struct.node** @llvm.object.stage.p0p0s_struct.nodes

// CHECK-LABEL: define %struct.node* @fieldstage2(%struct.node* %s)
struct node *fieldstage2(struct node *s) {
  return s->x;
}

struct node1 {
  int x[2] __stage(0);
};

// CHECK-LABEL: define i32 @fieldstage3(%struct.node1* %s)
int fieldstage3(struct node1 *s) __stage(1) {
  // CHECK: [[gep:%.+]] = getelementptr
  // CHECK-NEXT: [[x:%.+]] = call [2 x i32]* @llvm.object.stage.p0a2i32([2 x i32]* [[gep]], i32 0)
  // CHECK-NEXT: [[xgep:%.+]] = getelementptr inbounds [2 x i32], [2 x i32]* [[x]], i64 0, i64 0
  // CHECK-NEXT: load i32, i32* [[xgep]]
  return s->x[0];
}

// CHECK: declare [2 x i32]* @llvm.object.stage.p0a2i32

// CHECK: {{^}}attributes [[$attr]] = {{.*}} stage=2
