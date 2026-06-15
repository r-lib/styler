# Replace single quotes with double quotes

We do not use [`deparse()`](https://rdrr.io/r/base/deparse.html) as in
previous implementations but
[`paste0()`](https://rdrr.io/r/base/paste.html) since the former
approach escapes the reverse backslash in the line break character `\\n`
whereas the solution with
[`paste0()`](https://rdrr.io/r/base/paste.html) does not.

## Usage

``` r
fix_quotes(pd_flat)
```

## Arguments

- pd_flat:

  A flat parse table.

## Examples

``` r
style_text("'here
is a string
'")
#> "here
#> is a string
#> "
```
