
<!-- README.md is generated from README.Rmd. Please edit that file -->

# styler

[![Build
Status](https://travis-ci.org/r-lib/styler.svg?branch=master)](https://travis-ci.org/r-lib/styler)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/r-lib/styler?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/styler)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/r-lib/styler/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/styler)
[![cran
version](http://www.r-pkg.org/badges/version/styler)](https://cran.r-project.org/package=styler)

The goal of styler is to provide non-invasive pretty-printing of R
source code while adhering to the
[tidyverse](https://github.com/tidyverse/style) formatting rules.
Support for custom style guides is planned.

You can install the package from CRAN:

``` r
install.packages("styler")
```

Or get the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("r-lib/styler")
```

You can style a simple character vector of code with `style_text()`:

``` r
ugly_code <- "a<-function( x){1+1}           "
style_text(ugly_code)
#> a <- function(x) {
#>   1 + 1
#> }
```

There are a few variants of `style_text()`:

  - `style_file()` styles .R and/or .Rmd files.
  - `style_dir()` styles all .R files in a directory.
  - `style_pkg()` styles the source files of an R package.
  - RStudio Addins for styling the active file, styling the current
    package and styling the highlighted code
region.

<img src="https://raw.githubusercontent.com/lorenzwalthert/some_raw_data/master/styler_0.1.gif" width="650px" />

You can find more information on the wiki of [Google Summer of
Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Noninvasive-source-code-formatting)
or check out the [pkgdown](https://r-lib.github.io/styler/) page.
