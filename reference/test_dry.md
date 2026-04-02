# Test the dry argument

Test the dry argument

## Usage

``` r
test_dry(path, styler, styled = FALSE)
```

## Arguments

- path:

  A path to pass to the `styler`.

- styler:

  A function that takes `path`, typically a user exposed styler function
  that has side effects, like
  [`style_file()`](https://styler.r-lib.org/reference/style_file.md).
