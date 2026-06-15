# Set spacing of token to a certain level

Set the spacing of all `tokens` in `pd_flat` to `level` if
`strict = TRUE` or to at least to `level` if `strict = FALSE`.

## Usage

``` r
style_space_around_token(
  pd_flat,
  strict,
  tokens,
  level_before,
  level_after = level_before
)
```

## Arguments

- pd_flat:

  A nest or a flat parse table.

- strict:

  Whether the rules should be applied strictly or not.

- tokens:

  Character vector with tokens that should be styled.

- level_before, level_after:

  Scalar indicating the amount of spaces that should be inserted around
  the `tokens` on the left and right position respectively.
