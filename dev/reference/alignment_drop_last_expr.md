# Remove last expression

In a *nest*, if the last token is an `expr`, the *nest* represents
either an if, while or for statement or a function call. We don't call
about that part, in fact it's important to remove it for alignment. See
'Examples'.

## Usage

``` r
alignment_drop_last_expr(pds_by_line)
```

## Examples

``` r
if (FALSE) {
call(
  x = 12,
  y =  3,
)

function(a = 33,
         qq = 4) {
  # we don't care about this part for alignment detection
}
}
```
