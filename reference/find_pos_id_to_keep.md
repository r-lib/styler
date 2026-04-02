# Find the pos ids to keep

To make a parse table shallow, we must know which ids to keep.
`split(cumsum(pd_parent_first$parent == 0L))` above puts comments with
negative parents in the same block as proceeding expressions (but also
with positive). `find_pos_id_to_keep()` must hence always keep negative
comments. We did not use `split(cumsum(pd_parent_first$parent < 1L))`
because then every top-level comment is an expression on its own and
processing takes much longer for typical roxygen annotated code.

## Usage

``` r
find_pos_id_to_keep(pd)
```

## Arguments

- pd:

  A temporary top-level nest where the first expression is always a
  top-level expression, potentially cached.

## Details

Note that top-level comments **above** code have negative parents (the
negative value of the parent of the code expression that follows after,
another comment might be in the way though), all comments that are not
top level have positive ids. All comments for which no code follows
afterwards have parent 0.

## Examples

``` r
styler:::get_parse_data(c("#", "1"))
#>   line1 col1 line2 col2 id parent     token terminal text pos_id short
#> 1     1    1     1    1  1     -5   COMMENT     TRUE    #      1     #
#> 2     2    1     2    1  4      5 NUM_CONST     TRUE    1      2     1
#> 3     2    1     2    1  5      0      expr    FALSE    1      3     1
styler:::get_parse_data(c("c(#", "1)"))
#>   line1 col1 line2 col2 id parent                token terminal    text pos_id
#> 1     1    1     2    2 12      0                 expr    FALSE c(#\n1)      1
#> 2     1    1     1    1  1      3 SYMBOL_FUNCTION_CALL     TRUE       c      2
#> 3     1    1     1    1  3     12                 expr    FALSE       c      3
#> 4     1    2     1    2  2     12                  '('     TRUE       (      4
#> 5     1    3     1    3  4     12              COMMENT     TRUE       #      5
#> 6     2    1     2    1  6      7            NUM_CONST     TRUE       1      6
#> 7     2    1     2    1  7     12                 expr    FALSE       1      7
#> 8     2    2     2    2  8     12                  ')'     TRUE       )      8
#>    short
#> 1 c(#\n1
#> 2      c
#> 3      c
#> 4      (
#> 5      #
#> 6      1
#> 7      1
#> 8      )
styler:::get_parse_data(c("", "c(#", "1)", "#"))
#>   line1 col1 line2 col2 id parent                token terminal    text pos_id
#> 1     2    1     3    2 14      0                 expr    FALSE c(#\n1)      1
#> 2     2    1     2    1  3      5 SYMBOL_FUNCTION_CALL     TRUE       c      2
#> 3     2    1     2    1  5     14                 expr    FALSE       c      3
#> 4     2    2     2    2  4     14                  '('     TRUE       (      4
#> 5     2    3     2    3  6     14              COMMENT     TRUE       #      5
#> 6     3    1     3    1  8      9            NUM_CONST     TRUE       1      6
#> 7     3    1     3    1  9     14                 expr    FALSE       1      7
#> 8     3    2     3    2 10     14                  ')'     TRUE       )      8
#> 9     4    1     4    1 18      0              COMMENT     TRUE       #      9
#>    short
#> 1 c(#\n1
#> 2      c
#> 3      c
#> 4      (
#> 5      #
#> 6      1
#> 7      1
#> 8      )
#> 9      #
```
