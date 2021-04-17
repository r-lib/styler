
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {styler}

<!-- badges: start -->

[![R build
status](https://github.com/r-lib/styler/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/styler/actions)
[![Life cycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov test
coverage](https://codecov.io/gh/r-lib/styler/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/styler)
[![CRAN
Status](https://www.r-pkg.org/badges/version/styler)](https://cran.r-project.org/package=styler)
<!-- badges: end -->

# Overview

styler formats your code according to the [tidyverse style
guide](https://style.tidyverse.org) (or your custom style guide) so you
can direct your attention to the content of your code. It helps to keep
the coding style consistent across projects and facilitate
collaboration. You can access {styler} through

-   the RStudio Addin as demonstrated below
-   R functions like `style_pkg()`, `style_file()` or `style_text()`
-   various other tools described in
    `vignette("third-party-integrations")`

<img src="https://raw.githubusercontent.com/lorenzwalthert/some_raw_data/master/styler_0.1.gif" width="650px" />

## Installation

You can install the package from CRAN.

``` r
install.packages("styler")
```

Or get the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("r-lib/styler")
```

If you don’t use {styler} interactively (i.e. not from the R prompt or
RStudio Addin), make sure you authorize the R package R.cache once to
set up a permanent cache. If you use it interactively, you will be asked
to grant this permission once. See `?caching` for details.

## Documentation

The following online docs are available:

-   [latest CRAN release](https://styler.r-lib.org).

-   [GitHub development version](https://styler.r-lib.org/dev/).
