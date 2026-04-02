# Check whether a round trip verification can be carried out

If scope was set to "line_breaks" or lower (compare
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)),
we can compare the expression before and after styling and return an
error if it is not the same.

## Usage

``` r
parse_tree_must_be_identical(transformers)
```

## Arguments

- transformers:

  The list of transformer functions used for styling. Needed for reverse
  engineering the scope.
