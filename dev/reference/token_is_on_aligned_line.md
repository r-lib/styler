# Check if tokens are aligned

If all tokens are aligned, `TRUE` is returned, otherwise `FALSE`. The
function only checks for alignment of function calls. This can be
recycled conveniently later if needed as a vector with length \> 1.

## Usage

``` r
token_is_on_aligned_line(pd_flat)
```

## Arguments

- pd_flat:

  A flat parse table.

## Details

Multiple lines are called aligned if the following conditions hold for
all but the first line of the expression:

- lag spaces of column 1 must agree.

- spacing around comma (0 before, \> 1 after) and spacing around `=` (at
  least one around).

- all positions of commas of col \> 2 must agree (needs recursive
  creation of `text`).

Because of the last requirement, this function is very expensive to run.
For this reason, the following approach is taken:

- Only invoke the function when certain that alignment is possible.

- Check the cheap conditions first.

- For the recursive creation of text, greedily check column by column to
  make sure we can stop as soon as we found that columns are not
  aligned.

## Examples

``` r
library("magrittr")
withr::with_options(
  list(styler.cache_name = NULL), # temporarily deactivate cache
  {
    transformers <- tidyverse_style()
    pd_nested <- compute_parse_data_nested(c(
      "call(",
      "  ab = 1L,",
      "  a  = 2",
      ")"
    )) %>%
      styler:::post_visit(transformers$initialize)
    nest <- pd_nested$child[[1L]]
    styler:::token_is_on_aligned_line(nest)
  }
)
#> [1] TRUE
```
