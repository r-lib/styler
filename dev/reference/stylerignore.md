# Turn off styling for parts of the code

Using stylerignore markers, you can temporarily turn off styler. Beware
that for `styler > 1.2.0`, some alignment is [detected by
styler](https://styler.r-lib.org/articles/detect-alignment.html), making
stylerignore redundant. See a few illustrative examples below.

## Details

Styling is on for all lines by default when you run styler.

- To mark the start of a sequence where you want to turn styling off,
  use `# styler: off`.

- To mark the end of this sequence, put `# styler: on` in your code.
  After that line, styler will again format your code.

- To ignore an inline statement (i.e. just one line), place
  `# styler: off` at the end of the line. To use something else as start
  and stop markers, set the R options `styler.ignore_start` and
  `styler.ignore_stop` using
  [`options()`](https://rdrr.io/r/base/options.html). For styler version
  \> 1.6.2, the option supports character vectors longer than one and
  the marker are not exactly matched, but using a regular expression,
  which means you can have multiple marker on one line, e.g.
  `# nolint start styler: off`.

## Examples

``` r
# as long as the order of the markers is correct, the lines are ignored.
style_text(
  "
  1+1
  # styler: off
  1+1
  # styler: on
  1+1
  "
)
#> 1 + 1
#>   # styler: off
#>   1+1
#> # styler: on
#> 1 + 1

# if there is a stop marker before a start marker, styler won't be able
# to figure out which lines you want to ignore and won't ignore anything,
# issuing a warning.
if (FALSE) { # \dontrun{
style_text(
  "
  1+1
  # styler: off
  1+1
  # styler: off
  1+1
  "
)
} # }
# some alignment of code is detected, so you don't need to use stylerignore
style_text(
  "call(
    xyz =  3,
    x   = 11
  )"
)
#> call(
#>   xyz =  3,
#>   x   = 11
#> )
```
