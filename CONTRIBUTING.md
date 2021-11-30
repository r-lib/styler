# General

Before making a pull request, discuss your ideas in an issue. This repo is
maturing.

# Adding new hooks

To create a new hook, have a look at the [official
documentation](https://pre-commit.com/#new-hooks) on creating new hooks, then have a look
at existing hooks in this repo. The actual executables are defined in [`inst/hooks/`](https://github.com/lorenzwalthert/precommit/tree/main/inst/hooks). In
the script, you can expect the passed command line arguments to be all options, 
finally the files that should be processed with the hook.

For the scripts to become a hook, they need to be *registered* in
[`.pre-commit-hooks.yaml`](https://github.com/lorenzwalthert/precommit/blob/main/.pre-commit-hooks.yaml). As of pre-commit 2.11, R is a [supported language of
pre-commit](https://pre-commit.com/#r). Hence, it should have `language: r` in `.pre-commit-hooks.yaml` and then (for compatibility) a shebang in the `entrypoint` script.

# Testing hooks

Hooks should be tested by checking both the positive outcome (hook passes) and
the negative outcome (hook fails) by adding two `run_test()` statements to
[`./tests/testthat/test-hooks.R`](https://github.com/lorenzwalthert/precommit/blob/main/tests/testthat/test-hooks.R). Look at existing examples and [the documentation
of `run_test()`](https://lorenzwalthert.github.io/precommit/reference/run_test.html). Note that this won't interact with pre-commit. It will simply
run `Rscript path/to/script.R` (whereas with pre-commit, a {renv} will be activated before running the script). Also, there are [tests](https://github.com/lorenzwalthert/precommit/blob/main/.github/workflows/end-to-end.yml) to ensure that hooks are correctly registered in `.pre-commit-hooks.yaml`.

# Summary

- add your R (with extension) script in `inst/hooks/exported` and make it executable. See other scripts in `inst/hooks/exported` for a starting point for setting up your script.

- register hook in `.pre-commit-hooks.yaml`.

- add two unit tests, test manually with `pre-commit try-repo` and adapt the [end-to-end test](https://github.com/lorenzwalthert/precommit/blob/main/.github/workflows/end-to-end.yml).

- add a description of the new hook to `vignettes/available-hooks.Rmd`. Both description and
  `yaml` example code.
