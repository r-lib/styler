# Apply a function to the contents of a file

Transforms a file with a function.

## Usage

``` r
transform_utf8(path, fun, dry)
```

## Arguments

- path:

  A vector with file paths to transform.

- fun:

  A function that returns a character vector.

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.
