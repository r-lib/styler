# Nest a flat parse table

`nest_parse_data` groups `pd_flat` into a parse table with tokens that
are a parent to other tokens (called internal) and such that are not
(called child). Then, the token in child are joined to their parents in
internal and all token information of the children is nested into a
column "child". This is done recursively until we are only left with a
nested data frame that contains one row: The nested parse table.

## Usage

``` r
nest_parse_data(pd_flat)
```

## Arguments

- pd_flat:

  A flat parse table including both terminals and non-terminals.

## Value

A nested parse table.

## See also

[`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md)
