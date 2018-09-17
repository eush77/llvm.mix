// RUN: %clang_cc1 -fsyntax-only -verify %s

int f(int a, int b) __stage(1) { return a + b; }

__attribute__((mix(f))) void *gen(void*, int, int);
__attribute__((mix_ir(f))) int gen_ir(void*, int, int);

int variable __attribute__((mix(f))); // expected-error{{'mix' attribute only applies to functions}}
int variable1 __attribute__((mix_ir(f))); // expected-error{{'mix_ir' attribute only applies to functions}}

int argument() __attribute__((mix)); // expected-error{{'mix' attribute takes one argument}}
int argument1() __attribute__((mix_ir)); // expected-error{{'mix_ir' attribute takes one argument}}
int argument2() __attribute__((mix())); // expected-error{{'mix' attribute takes one argument}}
int argument3() __attribute__((mix(1, 2))); // expected-error{{'mix' attribute takes one argument}}
int argument4() __attribute__((mix_ir(1, 2))); // expected-error{{'mix_ir' attribute takes one argument}}
int argument5() __attribute__((mix(0))); // expected-error{{'mix' argument does not refer to a function}}
int argument6() __attribute__((mix_ir(0))); // expected-error{{'mix_ir' argument does not refer to a function}}
int argument7() __attribute__((mix(variable))); // expected-error{{'mix' argument 'variable' does not refer to a function}}
int argument8() __attribute__((mix_ir(variable))); // expected-error{{'mix_ir' argument 'variable' does not refer to a function}}

int n(int a, int b);
void *nobody() __attribute__((mix_ir(n))); // expected-error{{'mix_ir' argument 'n' does not refer to a function with a body}}

int u(int a, int b) { return a + b; }
void *unstaged() __attribute__((mix_ir(u))); // expected-error{{'mix_ir' argument 'u' does not refer to a staged function}}

int rettype() __attribute__((mix_ir(f))); // expected-error{{Expected a pointer return type from a function with 'mix_ir' attribute}}
void rettype1() __attribute__((mix(f))); // expected-error{{Expected return type 'int' from a function with 'mix' attribute}}

int argcount(void*, int) __attribute__((mix_ir(f))); // expected-error{{Expected 3 arguments from a function with 'mix_ir' attribute}}

int argtype(
    int, // expected-error{{Context argument does not have a pointer type}}
    int,
    int) __attribute__((mix_ir(f)));
int argtype1(void *, int,
             char // expected-error{{Argument type does not match argument type 'int' of 'f'}}
             ) __attribute__((mix_ir(f)));

void *p() __stage(1) {}

__attribute__((mix(p),     // expected-error{{'mix' and 'mix_ir' attributes are not compatible}}
               mix_ir(p))) // expected-note{{conflicting attribute is here}}
void *
conflict(void *);

__attribute__((mix(f),  // expected-error{{'mix' and 'mix' attributes are not compatible}}
               mix(f))) // expected-note{{conflicting attribute is here}}
void *
conflict1(void *, int, int);

__attribute__((mix_ir(f),  // expected-error{{'mix_ir' and 'mix_ir' attributes are not compatible}}
               mix_ir(f))) // expected-note{{conflicting attribute is here}}
int
conflict2(void *, int, int);
