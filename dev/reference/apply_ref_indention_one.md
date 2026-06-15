# Applying reference indention of a target token

Applies the indention level of `target_token` to all tokens that have
`target_token` as a reference. This includes adding spaces to the first
tokens on a line and updating the column `col1` and `col2` for all
tokens on that line so they are kept updated.

## Usage

``` r
apply_ref_indention_one(flattened_pd, target_token)
```

## Arguments

- flattened_pd:

  A flattened parse table

- target_token:

  The index of the token from which the indention level should be
  applied to other tokens.
