// RUN: %clang_cc1 -fsyntax-only -verify %s

int f(int __stage(1) x);

__stage int syntax0; // expected-error {{expected '('}}
int syntax1(int __stage x); // expected-error {{expected '('}}

__stage(1) int subject0; // expected-warning {{only applies to parameters}}
int __stage(1) subject1; // expected-warning {{only applies to parameters}}

int argument0(int __stage(f(0)) x); // expected-error {{requires an integer constant}}
int argument1(int __stage("static") x); // expected-error {{requires an integer constant}}
int argument2(int __stage(-1) x); // expected-error {{cannot be represented in a 32-bit signed integer type}}
int argument3(int __stage(1 << 32) x); // expected-error {{cannot be represented in a 32-bit signed integer type}}
int argument4(int __stage(0) x); // expected-error {{must be greater than 0}}

int multiple(int __stage(1) __stage(2) x); // expected-error {{once per parameter}}
