
<!-- README.md is generated from README.Rmd. Please edit that file -->

# styler

[![Build
Status](https://travis-ci.org/r-lib/styler.svg?branch=master)](https://travis-ci.org/r-lib/styler)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/r-lib/styler?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/styler)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/r-lib/styler/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/styler)
[![cran
version](http://www.r-pkg.org/badges/version/styler)](https://cran.r-project.org/package=styler)

The goal of styler is to provide non-invasive pretty-printing of R
source code while adhering to the
[tidyverse](http://style.tidyverse.org) formatting rules. styler can be
customized to format code according to other style guides too.

## Installation

You can install the package from CRAN:

``` r
install.packages("styler") 
```

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

  - `style_file()` styles .R and/or .Rmd files.
  - `style_dir()` styles all .R and/or .Rmd files in a directory.
  - `style_pkg()` styles the source files of an R package.
  - RStudio Addins for styling the active file, styling the current
    package and styling the highlighted code
region.

<img src="https://raw.githubusercontent.com/lorenzwalthert/some_raw_data/master/styler_0.1.gif" width="650px" />

## Functionality of styler

**scope**

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

**strict**

If you wish to keep alignment as is, you can use `strict = FALSE`:

``` r
style_text(
  c(
    "first  <- 4", 
    "second <- 1+1"
  ),
  strict = FALSE 
) 
#> first  <- 4
#> second <- 1 + 1
```

This was just the tip of the iceberg. Learn more about customization
with the tidyverse style guide in in this
[vignette](http://styler.r-lib.org/articles/introducing_styler.html). If
this is not flexible enough for you, you can implement your own style
guide, as explained in the corresponding
[vignette](http://styler.r-lib.org/articles/customizing_styler.html).

## Adaption of styler

styler functionality is made available through other packages, most
notably

  - `usethis::use_tidy_style()` styles your project according to the
    tidyverse style guide.
  - `reprex::reprex(style = TRUE)` to prettify reprex code before
    printing. To permanently use `style = TRUE` without specifying it
    every time, you can add the following line to your `.Rprofile` (via
    `usethis::edit_r_profile()`): `options(reprex.styler = TRUE)`.
  - you can pretty-print your R code in RMarkdown reports without having
    styler modifying the source. This feature is implemented as a code
    chunk option in knitr. use `tidy = "styler"` in the header of a code
    chunks (e.g. ` ```{r name-of-the-chunk, tidy = "styler"}`), or
    `knitr::opts_chunk$set(tidy = "styler")` at the top of your
    RMarkdown script.
  - pretty-printing of [drake](https://github.com/ropensci/drake)
    workflow data frames with `drake::drake_plan_source()`.
  - Adding styler as a fixer to the [ale
    Plug-in](https://github.com/w0rp/ale/pull/2401#issuecomment-485942966)
    for VIM.

## Further resources

  - The official [web documentation](http://styler.r-lib.org/) of
    styler, containing various vignettes function documentation as well
    as a change-log.
  - [Blog
    post](https://lorenzwalthert.netlify.com/posts/customizing-styler-the-quick-way/)
    about how you can customize styler without being an expert.
  - A [tidyverse.org blog
    post](https://www.tidyverse.org/articles/2017/12/styler-1.0.0/)
    introducing the functionality of styler.
  - The wiki of [Google Summer of Code
    2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Noninvasive-source-code-formatting)
    or the [pkgdown](https://r-lib.github.io/styler/) page contain
    information related to the initial development phase during Google
    Summer of Code 2017.
