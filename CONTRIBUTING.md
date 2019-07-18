# General

Before making a pull request, discuss your ideas in an issue. This repo is 
experimental. It does not have a completely standard package structure (yet).
This repo uses the [tic](https://github.com/ropenscilabs/tic) package for CI.

# Adding new hooks

To create a new hook, have a look at the [official
documentation](https://pre-commit.com) on creating new hooks, then have a look
at existing hooks in this repo. The actual executables are defined in `bin/`. In
the script, you can expect the passed command line arguments to be the files
that should be processed with the hook.

For the scripts to become a hook, they need to be *registered* in
`.pre-commit-hooks.yaml`. As R is not currently a supported language of
pre-commit (https://github.com/pre-commit/pre-commit/issues/926), most hooks use
`language: script` and then a shebang in the `entrypoint` script.

Hooks should be tested by checking both the positive outcome (hook passes) and
the negative outcome (hook fails) by adding two `run_test()` statements to
`./tests/testthat/test-all.R`. Look at existing examples and the documentation of
`run_test()`.

To summarize: 

- add executable in `bin`.
- register hook in `.pre-commit-hooks.yaml`.
- add two tests.
- add a description of the new hook to the `README.Rmd`.
