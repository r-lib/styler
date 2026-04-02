# Flatten a parse table

Flattens a parse table if certain tokens occur in this table or its
child, either flattening from left or from right. If one of `token` is
present in `pd_nested` and one of `child_token` is present in one of the
children next to `token` in `pd_nested`, the nested parse table is
flattened. Otherwise, it is returned unmodified.

## Usage

``` r
flatten_pd(pd_nested, token, child_token = token, left = TRUE)
```

## Arguments

- pd_nested:

  A nested parse table.

- token:

  A character vector with tokens of which at least one has to occur in
  `pd_nested` in order to flatten it.

- child_token:

  A character vector of tokens of which at least one has to occur in the
  child in order to flatten the parse table.

- left:

  Flag that indicates whether the parse table should be flattened from
  left or from right.
