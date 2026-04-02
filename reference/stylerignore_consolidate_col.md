# Consolidate columns after a merge

After [`base::merge()`](https://rdrr.io/r/base/merge.html), all non-id
columns that were present in `x` and `y` do get a suffix `.x` and `.y`.
If the `y` value is missing, use the `x` value (because the information
for this token was not stylerignored), otherwise the `y` value (i.e. the
styled value).

## Usage

``` r
stylerignore_consolidate_col(
  flattened_pd,
  col,
  col_x = paste0(col, ".x"),
  col_y = paste0(col, ".y")
)
```

## Arguments

- flattened_pd:

  A flattened parse table.

- col:

  A string indicating the name of the column that should be
  consolidated.

- col_x, col_y:

  The name of the column from the left (right) parent to consolidate.
