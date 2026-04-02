# Verify the styling

If scope was set to "line_breaks" or lower (compare
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)),
we can compare the expression before and after styling and return an
error if it is not the same. If that's not possible, a weaker guarantee
that we want to give is that the resulting code is parsable.

## Usage

``` r
verify_roundtrip(old_text, new_text, parsable_only = FALSE)
```

## Arguments

- old_text:

  The initial expression in its character representation.

- new_text:

  The styled expression in its character representation.

- parsable_only:

  If we should only check for the code to be parsable.

## Limitation

Note that this method ignores roxygen code examples and comments and no
verification can be conducted if tokens are in the styling scope.

## Examples

``` r
styler:::verify_roundtrip("a+1", "a + 1")
styler:::verify_roundtrip("a+1", "a + 1 # comments are dropped")
try(styler:::verify_roundtrip("a+1", "b - 3"))
#> Error in styler:::verify_roundtrip("a+1", "b - 3") : 
#>   The expression evaluated before the styling is not the same as the expression after styling. This should not happen.
#> ℹ This is an internal error that was detected in the styler package.
#>   Please report it at <https://github.com/r-lib/styler/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.
```
