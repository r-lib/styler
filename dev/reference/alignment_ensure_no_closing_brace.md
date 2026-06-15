# Ensure the closing brace of the call is removed

Must be after dropping comments because the closing brace is only
guaranteed to be the last token in that case.

## Usage

``` r
alignment_ensure_no_closing_brace(pd_by_line, last_line_droped_early)
```

## Arguments

- pd_by_line:

  A list, each element corresponding to a potentially incomplete parse
  table that represents all token from one line.
