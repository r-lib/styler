# `dir()`, but without dot-prefix and different defaults

When using [`dir()`](https://rdrr.io/r/base/list.files.html), you can
set `full.names = FALSE`, but then you can only pass a character vector
of length one as `path` to not loose the information about where the
files are. This function solves that case. It's needed when one wants to
standardize paths to use set operations on them, i.e. when the user
supplied input does not have a dot prefix. See 'Examples'.

## Usage

``` r
dir_without_.(path, recursive = TRUE, ...)
```

## Arguments

- path:

  A path.

- ...:

  Passed to [`base::dir()`](https://rdrr.io/r/base/list.files.html).

## Details

For different defaults, see `dir_without_._one`.

## See also

set_and_assert_arg_paths

## Examples

``` r
setdiff("./file.R", "file.R") # you want to standardize first.
#> [1] "./file.R"
```
