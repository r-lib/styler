# Find the index of the next or previous non-comment in a parse table.

Find the index of the next or previous non-comment in a parse table.

## Usage

``` r
next_non_comment(pd, pos)

previous_non_comment(pd, pos)
```

## Arguments

- pd:

  A parse table.

- pos:

  The position of the token to start the search from.

## See also

Other third-party style guide helpers:
[`pd_is`](https://styler.r-lib.org/reference/pd_is.md),
[`scope_normalize()`](https://styler.r-lib.org/reference/scope_normalize.md)

## Examples

``` r
code <- "a <- # hi \n x %>% b()"
writeLines(code)
#> a <- # hi 
#>  x %>% b()
pd <- compute_parse_data_nested(code)
child <- pd$child[[1]]
previous_non_comment(child, 4L)
#> [1] 2
next_non_comment(child, 2L)
#> [1] 4
```
