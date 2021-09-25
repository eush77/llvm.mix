// RUN: %clang_cc1 -fsyntax-only -verify %s

int f(int a, int b) __stage(1) { return a + b; }

__attribute__((mix(f))) void *gen(void*, int, int);

int variable __attribute__((mix(f))); // expected-error{{'mix' attribute only applies to functions}}

int argument0() __attribute__((mix)); // expected-error{{'mix' attribute takes one argument}}
int argument1() __attribute__((mix())); // expected-error{{'mix' attribute takes one argument}}
int argument2() __attribute__((mix(1, 2))); // expected-error{{'mix' attribute takes one argument}}
int argument3() __attribute__((mix(0))); // expected-error{{'mix' argument does not refer to a function}}
int argument4() __attribute__((mix(variable))); // expected-error{{'mix' argument 'variable' does not refer to a function}}

int n(int a, int b);
void *nobody() __attribute__((mix(n))); // expected-error{{'mix' argument 'n' does not refer to a function with a body}}

int u(int a, int b) { return a + b; }
void *unstaged() __attribute__((mix(u))); // expected-error{{'mix' argument 'u' does not refer to a staged function}}

int rettype() __attribute__((mix(f))); // expected-error{{Expected a pointer return type from a function with 'mix' attribute}}

void *argcount(void*, int) __attribute__((mix(f))); // expected-error{{Expected 3 arguments from a function with 'mix' attribute}}

void *argtype(
    int, // expected-error{{Context argument does not have a pointer type}}
    int,
    int) __attribute__((mix(f)));
void *argtype1(void *, int,
               char // expected-error{{Argument type does not match argument type 'int' of 'f'}}
               ) __attribute__((mix(f)));

__attribute__((mix(f),  // expected-note{{conflicting attribute is here}}
               mix(f))) // expected-error{{'mix' and 'mix' attributes are not compatible}}
void *
conflict(void *, int, int);
