# Remove all comment tokens

Must be after split by line because it invalidates (lag)newlines, which
are used for splitting by line.

## Usage

``` r
alignment_drop_comments(pd_by_line)
```

## Arguments

- pd_by_line:

  A list, each element corresponding to a potentially incomplete parse
  table that represents all token from one line.
