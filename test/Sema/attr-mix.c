// RUN: %clang_cc1 -fsyntax-only -verify %s

int f(int a, int b) { return a + b; }

__attribute__((mix(f))) void *gen(void*, int);
__attribute__((mix_ir(f))) void *gen_ir(void*, int);

int variable __attribute__((mix(f))); // expected-error{{'mix' attribute only applies to functions}}
int variable1 __attribute__((mix_ir(f))); // expected-error{{'mix_ir' attribute only applies to functions}}

int argument() __attribute__((mix)); // expected-error{{'mix' attribute takes one argument}}
int argument1() __attribute__((mix_ir)); // expected-error{{'mix_ir' attribute takes one argument}}
int argument2() __attribute__((mix())); // expected-error{{'mix' attribute takes one argument}}
int argument3() __attribute__((mix(1, 2))); // expected-error{{'mix' attribute takes one argument}}
int argument4() __attribute__((mix_ir(1, 2))); // expected-error{{'mix_ir' attribute takes one argument}}
int argument5() __attribute__((mix(0))); // expected-error{{'mix' argument is not a function}}
int argument6() __attribute__((mix_ir(0))); // expected-error{{'mix_ir' argument is not a function}}
int argument7() __attribute__((mix(variable))); // expected-error{{'mix' argument 'variable' is not a function}}
int argument8() __attribute__((mix_ir(variable))); // expected-error{{'mix_ir' argument 'variable' is not a function}}

__attribute__((mix(f),     // expected-error{{'mix' and 'mix_ir' attributes are not compatible}}
               mix_ir(f))) // expected-note{{conflicting attribute is here}}
int
conflict();

__attribute__((mix(f),// expected-error{{'mix' and 'mix' attributes are not compatible}}
               mix(f))) // expected-note{{conflicting attribute is here}}
int conflict1();

__attribute__((mix_ir(f),// expected-error{{'mix_ir' and 'mix_ir' attributes are not compatible}}
               mix_ir(f))) // expected-note{{conflicting attribute is here}}
int conflict2();
