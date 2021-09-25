; RUN: %lli -force-interpreter %s

define { i32, i32 } @f() {
  ret { i32, i32 } { i32 0, i32 0 }
}

define i32 @main() {
  %s = call { i32, i32 } @f()
  %t = extractvalue { i32, i32 } %s, 0
  ret i32 %t
}
