# Parse, transform and serialize a nested parse table

We process blocks of nested parse tables for speed. See
[`cache_find_block()`](https://styler.r-lib.org/reference/cache_find_block.md)
for details on how a top-level nest is split into blocks.

## Usage

``` r
parse_transform_serialize_r_block(
  pd_nested,
  start_line,
  transformers,
  base_indention
)
```

## Arguments

- pd_nested:

  A block of the nested parse table.

- start_line:

  The line number on which the code starts.

- transformers:

  A list of *named* transformer functions

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

## Examples

``` r
text_in <- 'x<- function()
"here
is"
NULL
1+ 1
'
style_text(text_in, base_indention = 3)
#>    x <- function() {
#>      "here
#> is"
#>    }
#>    NULL
#>    1 + 1
# not equal to the naive approach
styler:::construct_vertical(
  paste0(styler:::add_spaces(3), style_text(text_in), sep = "")
)
#>    x <- function() {
#>      "here
#>    is"
#>    }
#>    NULL
#>    1 + 1
```
