// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s --enable-var-scope --implicit-check-not=object.stage

// CHECK-LABEL: define i32 @staged
int staged(struct { int x, y; } __attribute__((staged)) * s) __stage(1) {
  // CHECK: [[x:%.+]] = getelementptr {{.*}}, i32 0
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[x]], i32 0)
  // CHECK: [[y:%.+]] = getelementptr {{.*}}, i32 1
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[y]], i32 0)
  return s->x + s->y;
}

// CHECK: declare i32* @llvm.object.stage.p0i32

// CHECK-LABEL: define i32 @staged
int staged1(struct { int x, y __stage(1); } __attribute__((staged)) * s) __stage(1) {
  // CHECK: [[x:%.+]] = getelementptr {{.*}}, i32 0
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[x]], i32 0)
  // CHECK: [[y:%.+]] = getelementptr {{.*}}, i32 1
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[y]], i32 1)
  return s->x + s->y;
}

// CHECK-LABEL: define i32 @nonstaged
int nonstaged(struct { int x, y; } * s) __stage(1) {
  return s->x + s->y;
}

// CHECK-LABEL: define i32 @implicit
int implicit(struct { int x, y __stage(1); } * s) __stage(1) {
  // CHECK: [[x:%.+]] = getelementptr {{.*}}, i32 0
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[x]], i32 0)
  // CHECK: [[y:%.+]] = getelementptr {{.*}}, i32 1
  // CHECK-NEXT: call i32* @llvm.object.stage.p0i32(i32* [[y]], i32 1)
  return s->x + s->y;
}
