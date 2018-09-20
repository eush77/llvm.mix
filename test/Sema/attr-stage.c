// RUN: %clang_cc1 -fsyntax-only -verify %s

__stage(1) int f(int __stage(1) x) __stage(1);

struct s {
  int x __stage(1);
};

__stage int syntax0; // expected-error {{expected '('}}
int syntax1(int __stage x); // expected-error {{expected '('}}

__stage(1) int subject0; // expected-warning {{only applies to functions, parameters, and non-static data members}}
int __stage(1) subject1; // expected-warning {{only applies to functions, parameters, and non-static data members}}

int argument0(int __stage(f(0)) x); // expected-error {{requires an integer constant}}
int argument1(int __stage("static") x); // expected-error {{requires an integer constant}}
int argument2(int __stage(-1) x); // expected-error {{cannot be represented in a 32-bit signed integer type}}
int argument3(int __stage(1 << 32) x); // expected-error {{cannot be represented in a 32-bit signed integer type}}

__stage(1) void voidtype(); // expected-error {{does not apply to void return values}}

int multiple(int __stage(1) __stage(2) x) __stage(2); // expected-error {{once per parameter}}
int multiple1() __stage(1) __stage(2); // expected-error {{expected function body}}
struct multiple2 {
  int x __stage(1) __stage(2); // expected-error {{expected ';'}}
};

int incompatible(int __stage(1) x); // expected-error {{Function stage is incompatible with the stages of its arguments}}
__stage(1) int incompatible1(int x); // expected-error {{Function stage is incompatible with the stage of its return value}}
__stage(1) int incompatible2(int x) __stage(2); // expected-error {{Function stage is incompatible with the stages of its arguments}}

int attributes() __stage(1) __attribute__((noreturn));
struct attributes1 { int x __stage(1) __attribute__((aligned(8))); };
