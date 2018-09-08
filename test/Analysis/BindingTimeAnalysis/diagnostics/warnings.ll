; RUN: opt -disable-output -bta %s 2>&1 | FileCheck %s --implicit-check-not="{{[^ ]}}"

; CHECK: warning: Multiple stage(0) terminators of %entry:
; CHECK: br label %exit  ; in %left
; CHECK: note: The other terminator is moved to stage(1):
; CHECK: br label %exit  ; in %right
define i32 @manyterm(i1 stage(1) %x) stage(1) {
entry:
  br i1 %x, label %left, label %right
left:
  br label %exit
right:
  br label %exit
exit:
  ret i32 1
}

; CHECK: warning: Multiple stage(0) terminators of %fork:
; CHECK: ret i32 1  ; in %merge
; CHECK: note: The other terminator is moved to stage(1):
; CHECK: br label %id1  ; in %id
; CHECK: warning: Multiple stage(1) terminators of %fork1:
; CHECK: br label %id1  ; in %id
; CHECK: note: The other terminator is moved to stage(2):
; CHECK: br label %merge  ; in %merge1
; CHECK: warning: Multiple stage(1) terminators of %fork:
; CHECK: ret i32 1  ; in %merge
; CHECK: note: The other terminator is moved to stage(2):
; CHECK: br label %id1  ; in %id
define i32 @manyterm1(i1 stage(1) %x1, i1 stage(2) %x2) stage(2) {
fork:
  br i1 %x1, label %fork1, label %merge
fork1:
  br i1 %x2, label %id, label %merge1
id:
  br label %id1
id1:
  br label %merge1
merge1:
  br label %merge
merge:
  ret i32 1
}
