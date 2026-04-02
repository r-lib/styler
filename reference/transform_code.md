# Transform code from R, Rmd or Rnw files

A wrapper which initiates the styling of either R, Rmd or Rnw files by
passing the relevant transformer function for each case.

## Usage

``` r
transform_code(path, fun, ..., dry)
```

## Arguments

- path:

  A vector with file paths to transform.

- fun:

  A function that returns a character vector.

- ...:

  Further arguments passed to
  [`transform_utf8()`](https://styler.r-lib.org/reference/transform_utf8.md).

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.
