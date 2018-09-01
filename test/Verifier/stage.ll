; RUN: not opt -verify -disable-output %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

declare i32 @f(i32 %x, i32 stage(1) %n) stage(1)
declare stage(1) i32 @f1(i32 %x, i32 stage(1) %n) stage(2)
declare stage(1) i32 @f2(i32 %x, i32 stage(1) %n) #2

attributes #2 = { stage=2 }

; CHECK: Attribute 'stage' only applies to functions and parameters of staged functions
; CHECK-NEXT: void (i32)* @notstaged
declare void @notstaged(i32 stage(1))

; CHECK: Attribute 'stage' only applies to functions and parameters of staged functions
; CHECK-NEXT: i32 ()* @notstaged1
declare stage(1) i32 @notstaged1()

; CHECK: Wrong types for attribute: {{.*}}stage{{.*}}
; CHECK-NEXT: void (i32)* @retstage
declare stage(1) void @retstage(i32 stage(1)) stage(2)

; CHECK: Last stage of @laststage is not compatible with the stages of its arguments
; CHECK-NEXT: i32 (i32)* @laststage
declare i32 @laststage(i32) stage(2)

; CHECK: Last stage of @laststage1 is not compatible with the stages of its arguments
; CHECK-NEXT: i32 (i32)* @laststage1
declare i32 @laststage1(i32 stage(2)) stage(1)

; CHECK: Last stage of @laststage2 is not compatible with the stage of its return value
; CHECK-NEXT: i32 ()* @laststage2
declare stage(2) i32 @laststage2() stage(1)

; CHECK: {{.*}} error: input module is broken!
