# Ensure last pd has a trailing comma

Must be after
[`alignment_ensure_no_closing_brace()`](https://styler.r-lib.org/reference/alignment_ensure_no_closing_brace.md)
because if it comes after `alignment_ensure_trailing_comma()`, the last
expression would not be a brace, which would make removal complicated.

## Usage

``` r
alignment_ensure_trailing_comma(pd_by_line)
```

## Arguments

- pd_by_line:

  A list, each element corresponding to a potentially incomplete parse
  table that represents all token from one line.
