# Flatten some token in the nested parse table based on operators

Certain tokens are not placed optimally in the nested parse data with
[`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md).
For example, the token of arithmetic operations 1 + 1 + 1 should all be
on the same level of nesting since the indention is the same for all but
the first two terminals. Setting the indention correctly is easier to
achieve if they are put on the same level of nesting.

## Usage

``` r
flatten_operators(pd_nested)
```

## Arguments

- pd_nested:

  A nested parse table to partially flatten.
