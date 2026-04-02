# Obtain a nested parse table from a character vector

Parses `text` to a flat parse table and subsequently changes its
representation into a nested parse table with
[`nest_parse_data()`](https://styler.r-lib.org/reference/nest_parse_data.md).

## Usage

``` r
compute_parse_data_nested(
  text,
  transformers = tidyverse_style(),
  more_specs = NULL
)
```

## Arguments

- text:

  The text to parse.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- more_specs:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

## Value

A nested parse table. See
[`tokenize()`](https://styler.r-lib.org/reference/tokenize.md) for
details on the columns of the parse table.

## Examples

``` r
code <- "
ab     <- 1L # some comment
abcdef <- 2L
"
writeLines(code)
#> 
#> ab     <- 1L # some comment
#> abcdef <- 2L
#> 
compute_parse_data_nested(code)
#>   id pos_id line1 col1 line2 col2 parent   token terminal           text short
#> 1 10      1     2    1     2   12      0    expr    FALSE   ab     <- 1L ab   
#> 2  8      7     2   14     2   27    -10 COMMENT     TRUE # some comment # som
#> 3 19      8     3    1     3   12      0    expr    FALSE   abcdef <- 2L abcde
#>   token_before token_after stylerignore block is_cached internal
#> 1         <NA>        <NA>        FALSE     1     FALSE     TRUE
#> 2    NUM_CONST      SYMBOL        FALSE     1     FALSE     TRUE
#> 3         <NA>        <NA>        FALSE     1     FALSE     TRUE
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                              child
#> 1                          5, 4, 7, 3, 4, 6, 2, 2, 2, 1, 8, 11, 2, 2, 2, 2, 9, 12, 10, 10, 10, expr, LEFT_ASSIGN, expr, FALSE, TRUE, FALSE, ab, <-, 1L, ab, <-, 1L, NA, SYMBOL, NA, NA, NUM_CONST, NA, FALSE, FALSE, FALSE, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 2, 3, 2, 1, 2, 2, 5, SYMBOL, TRUE, ab, ab, , LEFT_ASSIGN, FALSE, NA, FALSE, FALSE, 5, 6, 2, 11, 2, 12, 7, NUM_CONST, TRUE, 1L, 1L, LEFT_ASSIGN, COMMENT, FALSE, NA, FALSE, FALSE
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                             NULL
#> 3 15, 14, 17, 10, 11, 13, 3, 3, 3, 1, 8, 11, 3, 3, 3, 6, 9, 12, 19, 19, 19, expr, LEFT_ASSIGN, expr, FALSE, TRUE, FALSE, abcdef, <-, 2L, abcde, <-, 2L, NA, SYMBOL, NA, NA, NUM_CONST, NA, FALSE, FALSE, FALSE, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 9, 13, 3, 1, 3, 6, 15, SYMBOL, TRUE, abcdef, abcde, COMMENT, LEFT_ASSIGN, FALSE, NA, FALSE, FALSE, 12, 16, 3, 11, 3, 12, 17, NUM_CONST, TRUE, 2L, 2L, LEFT_ASSIGN, , FALSE, NA, FALSE, FALSE
```
