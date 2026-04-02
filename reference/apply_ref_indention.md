# Apply reference indention to tokens

Applies the reference indention created with functions
[`update_indention_ref()`](https://styler.r-lib.org/reference/update_indention_ref.md)
to the flattened parse table. The indention is applied to all token that
inherit from a reference token sequentially, i.e. by looping over the
target tokens.

## Usage

``` r
apply_ref_indention(flattened_pd)
```

## Arguments

- flattened_pd:

  A flattened parse table
