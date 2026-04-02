# `style_text()` without rules for `\{\{`

This function mocks
[`style_text()`](https://styler.r-lib.org/reference/style_text.md), but
without taking into consideration the rules for the curly-curly
syntactic sugar (introduced in rlang 0.4). This function
(`style_text_without_curly_curly()`) is needed for testing only, namely
to test indention with multiple curly braces in a sequence. It is
important to maintain testing for indention rules even as the
curly-curly expression is always kept on the same line in the tidyverse
style guide because we should ensure the underlying mechanics for
indention work correctly. When indention mechanisms are changed later,
e.g. by simplifying
[`compute_indent_indices()`](https://styler.r-lib.org/reference/compute_indent_indices.md),
we must have a way of testing this without the interaction of `\{\{`.

## Usage

``` r
style_text_without_curly_curly(
  text,
  ...,
  style = tidyverse_style,
  transformers = style(...),
  include_roxygen_examples = TRUE
)
```

## See also

set_line_break_around_curly_curly

## Examples

``` r
styler:::style_text_without_curly_curly("rlang::list2({{ x }} := 2L)")
#> rlang::list2({
#>   {
#>     x
#>   }
#> } := 2L)
styler:::style_text("rlang::list2({{ x }} := 3)")
#> rlang::list2({{ x }} := 3)
```
