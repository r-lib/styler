
<!-- README.md is generated from README.Rmd. Please edit that file -->

# styler

[![R build
status](https://github.com/r-lib/styler/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/styler/actions)

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/r-lib/styler/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/styler)
[![cran
version](https://www.r-pkg.org/badges/version/styler)](https://cran.r-project.org/package=styler)

styler formats your code according to the [tidyverse style
guide](https://style.tidyverse.org)[1] so you can direct your attention
to the content of your code. styler helps to keep the coding style
consistent across projects and facilitate collaboration.

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

If you don’t use styler interactively (i.e. not from the R prompt or
RStudio Addin), make sure you authorize `{R.cache}` once to set up a
permanent cache. If you use it interactively, you will be asked to grant
this permission once. See `?caching` for details.

## Documentation

The following online docs are available:

-   [latest CRAN release](https://styler.r-lib.org).

-   [GitHub development version](https://styler.r-lib.org/dev).

[1] or your custom style guide
