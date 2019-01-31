# llvm.mix

This is a fork of LLVM with `llvm.mix` extension that implements multi-stage
specializer generation as part of the LLVM pass pipeline.

This is a research project and a WIP.

# Usage

The extension has two main components: binding-time analysis and
specialization.

To print results of the binding-time analysis for functions annotated with
`stage` attributes, use `opt -analyze`:

    $ opt -analyze -bta tmp.ll

To generate specializer for the code containing the calls to `@llvm.mix`
intrinsic, include Mix pass in the pipeline:

    $ opt -S -mix tmp.ll

Please refer to [mix-examples] repository and test directories
([BTA][bta-tests], [Mix][mix-tests]) for more info.

# Links

- [Extension to the Clang front-end][clang.mix]
- [Examples and micro benchmarks][mix-examples]

[clang.mix]: https://github.com/eush77/clang.mix
[mix-examples]: https://github.com/eush77/mix-examples
[bta-tests]: https://github.com/eush77/llvm.mix/blob/mix/test/Analysis/BindingTimeAnalysis/
[mix-tests]: https://github.com/eush77/llvm.mix/blob/mix/test/Transforms/Mix/
