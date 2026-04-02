# Style spacing around math tokens

Style spacing around math tokens

## Usage

``` r
style_space_around_math_token(strict, zero, one, pd_flat)
```

## Arguments

- strict:

  Whether the rules should be applied strictly or not.

- zero:

  Character vector of tokens that should be surrounded with zero spaces.

- one:

  Character vector with tokens that should be surrounded by at least one
  space (depending on `strict = TRUE` in the styling functions
  [`style_text()`](https://styler.r-lib.org/reference/style_text.md) and
  friends). See 'Examples'.

- pd_flat:

  A nest or a flat parse table.
