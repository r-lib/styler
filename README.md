
<!-- README.md is generated from README.Rmd. Please edit that file -->

# styler

[![Build
Status](https://travis-ci.org/r-lib/styler.svg?branch=master)](https://travis-ci.org/r-lib/styler)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/r-lib/styler?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/styler)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/r-lib/styler/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/styler)
[![cran
version](https://www.r-pkg.org/badges/version/styler)](https://cran.r-project.org/package=styler)

The goal of styler is to provide non-invasive pretty-printing of R
source code while adhering to the
[tidyverse](https://style.tidyverse.org) formatting rules. styler can be
customized to format code according to other style guides too.

The following online docs are available:

  - [latest CRAN release](https://styler.r-lib.org).

  - [GitHub development version](https://styler.r-lib.org/dev).

## Installation

You can install the package from CRAN.

``` r
install.packages("styler")
```

If you don’t use styler interactively (i.e. not from the R prompt or
RStudio Addin), make sure you authorize `{R.cache}` once to set up a
permanent cache. If you use it interactively, you will be asked to grant
this permission once. See `?caching` for details.

Or get the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("r-lib/styler")
```

## API

You can style a simple character vector of code with `style_text()`:

``` r
library("styler")
ugly_code <- "a=function( x){1+1}           "
style_text(ugly_code)
#> a <- function(x) {
#>   1 + 1
#> }
```

There are a few variants of `style_text()`:

  - `style_file()` styles .R, .Rmd .Rnw and .Rprofile, files.

  - `style_dir()` styles all .R and/or .Rmd files in a directory.

  - `style_pkg()` styles the source files of an R package.

  - RStudio Addins for styling the active file, styling the current
    package and styling the highlighted code region.

<img src="https://raw.githubusercontent.com/lorenzwalthert/some_raw_data/master/styler_0.1.gif" width="650px" />

## Configuration

You can decide on the level of invasiveness with the scope argument. You
can style:

  - just spaces.

  - spaces and indention.

  - spaces, indention and line breaks.

  - spaces, indention, line breaks and tokens.

<!-- end list -->

``` r
ugly_code <- "a=function( x){1+1}           "
style_text(ugly_code, scope = "spaces")
#> a = function(x) {1 + 1}
```

Note that compared to the default used above `scope = "tokens"`:

  - no line breaks were added.

  - `<-` was not replaced with `=`.

While spaces still got styled (around `=` in `(x)`).

This was just the tip of the iceberg. To learn more about customization
options with the tidyverse style guide, see the [help file for
\`tidyverse\_style](https://styler.r-lib.org/reference/tidyverse_style.html)
for a quick overview or the [introductory
vignette](https://styler.r-lib.org/articles/introducing_styler.html).

## Features

  - style roxygen2 code examples.

  - do not re-style [deliberate code
    alignment](https://styler.r-lib.org/articles/detect-alignment.html).

  - [ignore some
    lines](https://styler.r-lib.org/dev/reference/stylerignore.html) for
    styling.

  - [cache styled
    expressions](https://styler.r-lib.org/dev/reference/caching.html)
    for speed.

## Adaption of styler

styler functionality is made available through other tools, most notably

  - as a pre-commit hook `style-files` in
    <https://github.com/lorenzwalthert/precommit>

  - `usethis::use_tidy_style()` styles your project according to the
    tidyverse style guide.

  - via commenting a PR on GitHub with `\style` when the [GitHub
    Action](https://github.com/features/actions) [*Tidyverse
    CI*](https://github.com/r-lib/actions/tree/master/examples#tidyverse-ci-workflow)
    is used. The most convenient way to set this up is via
    [`usethis::use_tidy_github_actions()`](https://usethis.r-lib.org/reference/tidyverse.html).

  - `reprex::reprex(style = TRUE)` to prettify reprex code before
    printing. To permanently use `style = TRUE` without specifying it
    every time, you can add the following line to your `.Rprofile` (via
    `usethis::edit_r_profile()`): `options(reprex.styler = TRUE)`.

  - you can pretty-print your R code in RMarkdown reports without having
    styler modifying the source. This feature is implemented as a code
    chunk option in knitr. use `tidy = "styler"` in the header of a code
    chunks (e.g. ` ```{r name-of-the-chunk, tidy = "styler"} `), or
    `knitr::opts_chunk$set(tidy = "styler")` at the top of your
    RMarkdown script.

  - Adding styler as a fixer to the [ale
    Plug-in](https://github.com/w0rp/ale/pull/2401#issuecomment-485942966)
    for VIM.

  - pretty-printing of [drake](https://github.com/ropensci/drake)
    workflow data frames with `drake::drake_plan_source()`.

  - Adding styler with
    [emacs-format-all-the-code](https://github.com/lassik/emacs-format-all-the-code)
    for Emacs.

## Further resources

  - The official [web documentation](https://styler.r-lib.org/) of
    styler, containing various vignettes function documentation as well
    as a change-log.

  - The wiki of [Google Summer of Code
    2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Noninvasive-source-code-formatting)
    or the [pkgdown](https://r-lib.github.io/styler/) page contain
    information related to the initial development phase during Google
    Summer of Code 2017.

## Contributing

Please have a look at `CONTRIBUTING.md`, in particular, make sure to use
the pre-commit hooks and if you skip a hook, describe why in the PR. See
the `{precommit}`
[README.md](https://github.com/lorenzwalthert/precommit) on how to
install the pre-commit framework and the R package on your system and
then run

``` r
precommit::use_precommit()
```

to make sure the hooks are activated in your local styler clone.
