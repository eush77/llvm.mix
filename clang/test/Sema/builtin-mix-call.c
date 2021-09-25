// RUN: %clang_cc1 -fsyntax-only -verify %s

void f() __stage(1) {}

__stage(1) void *ok(int x) __stage(1) {
  return __builtin_mix_call(f);
}

__stage(1) void *no_args(int x) __stage(1) {
  return __builtin_mix_call(); // expected-error{{too few arguments to function call, expected at least 1, have 0}}
}

__stage(1) void *callee_not_decl(int x) __stage(1) {
  return __builtin_mix_call("foo"); // expected-error{{'__builtin_mix_call' argument does not refer to a function}}
}

int *g;

__stage(1) void *callee_not_function(int x) __stage(1) {
  return __builtin_mix_call(g); // expected-error{{'__builtin_mix_call' argument 'g' does not refer to a function}}
}

void h();

__stage(1) void *callee_no_body(int x) __stage(1) {
  return __builtin_mix_call(h); // expected-error{{'__builtin_mix_call' argument 'h' does not refer to a function with a body}}
}

void i() {}

__stage(1) void *callee_not_staged(int x) __stage(1) {
  return __builtin_mix_call(i); // expected-error{{'__builtin_mix_call' argument 'i' does not refer to a staged function}}
}

__stage(1) void *argcount0(int x) __stage(1) {
  return __builtin_mix_call(f, 1); // expected-error{{Expected 1 arguments for a call to '__builtin_mix_call'}}
}

void j(int x) __stage(1) {}

__stage(1) void *argcount1(int x) __stage(1) {
  return __builtin_mix_call(j); // expected-error{{Expected 2 arguments for a call to '__builtin_mix_call'}}
}

__stage(1) void *argcount2(int x) __stage(1) {
  return __builtin_mix_call(f, 1, 2); // expected-error{{Expected 1 arguments for a call to '__builtin_mix_call'}}
}

void k(int x, ...) __stage(1) {}

__stage(1) void *argcount3(int x) __stage(1) {
  return __builtin_mix_call(k, 1, 2);
}

__stage(1) void *argtype(int x) __stage(1) {
  return __builtin_mix_call(j, &x); // expected-error{{Argument type does not match argument type 'int' of 'j'}}
}
