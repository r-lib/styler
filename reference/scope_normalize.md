# Convert the styling scope to its lower-level representation

If `scope` is of class `character` and of length one, the value of the
argument and all less-invasive levels are included too (e.g. styling
tokens includes styling spaces). If `scope` is of class `AsIs`, every
level to be included has to be declared individually. See compare
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)
for the possible levels and their order.

## Usage

``` r
scope_normalize(scope, name = substitute(scope))
```

## Arguments

- scope:

  A character vector of length one or a vector of class `AsIs`.

- name:

  The name of the character vector to be displayed if the construction
  of the factor fails.

## See also

Other third-party style guide helpers:
[`next_non_comment()`](https://styler.r-lib.org/reference/next_non_comment.md),
[`pd_is`](https://styler.r-lib.org/reference/pd_is.md)

## Examples

``` r
scope_normalize(I("tokens"))
#> [1] tokens
#> Levels: none < spaces < indention < line_breaks < tokens
scope_normalize(I(c("indention", "tokens")))
#> [1] indention tokens   
#> Levels: none < spaces < indention < line_breaks < tokens
```
