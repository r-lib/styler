
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Useful git pre-commit hooks for R related projects

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/lorenzwalthert/pre-commit-hooks.svg?branch=master)](https://travis-ci.org/lorenzwalthert/pre-commit-hooks)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

[Pre-commit hooks](https://pre-commit.com) are tests that run each time
you attempt to commit. If the tests pass, the commit will be made,
otherwise not. A very basic test is to check if the code is parsable,
making sure you have not forgotten a comma, brace or quote. The workflow
is best illustrated with an example:

![](https://media.giphy.com/media/Z9KJUt3zRYfhUDmLXG/giphy.gif)

## Installation

``` r
# once
remotes::install_github("lorenzwalthert/pre-commit-hooks")
precommithooks::install_precommit()

# in every project
precommithooks::use_precommit()
```

This installs pre-commit and performs some other set-up tasks. If you
want more control over the installation, see
`vignette("manual-installation")`.

# Usage

The next time you run `git commit`, the hooks listed in your
`.pre-commit-config.yaml` will get executed before the commit. The
helper function `precommithooks::open_config()` let’s you open the
`.pre-commit-config.yaml` conveniently from the RStudio console. When
any file is changed due to running a hook, the commit will fail. You can
inspect the changes introduced by the hook and if satisfied, you can
attempt to commit again. If all hooks pass, the commit is made. You can
also [temporarily disable
hooks](https://pre-commit.com/#temporarily-disabling-hooks). If you
succeed, it should look like
this:

![](/Users/lorenz/datasciencecoursera/pre-commit-hooks/man/figs/screenshot.png)<!-- -->

See the hooks provided by this repo under `vignette("available-hooks")`.
You can also add other hooks from other repos, by extending the
`.pre-commit-config.yaml` file, e.g. like this:

``` yaml
-   repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v1.2.3
    hooks: 
    -   id: check-added-large-files
```

To update the hook revisions, just run `pre-commit autoupdate` in your
terminal of `precommithooks::autoupdate()`.

# Caution

**Do not abort while hooks are running.** Non-staged changes are stashed
to a temp directory when the hooks are run and may not easily be
recovered afterwards.
