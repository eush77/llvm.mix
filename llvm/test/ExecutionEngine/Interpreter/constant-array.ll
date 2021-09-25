; RUN: %lli -force-interpreter %s

define [2 x i32] @f() {
  ret [2 x i32] [i32 0, i32 1]
}

define i32 @main() {
  %a = call [2 x i32] @f()
  %t = extractvalue [2 x i32] %a, 0
  ret i32 %t
}
