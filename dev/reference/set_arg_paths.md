# Standardize paths in root

Standardization required to use
[`setdiff()`](https://rdrr.io/r/base/sets.html) with paths.

## Usage

``` r
set_arg_paths(path)
```

## Arguments

- path:

  A path.

## See also

dir_without\_.

## Examples

``` r
styler:::set_arg_paths(c("./file.R", "file.R", "../another-file.R"))
#> [1] "file.R"            "file.R"            "../another-file.R"
```
