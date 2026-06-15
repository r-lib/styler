# Identify spinning code chunk header or xaringan

Wrongly identifies a comment without a preceding line break as a code
chunk header. See
https://yihui.name/knitr/demo/stitch/#spin-comment-out-texts for
details.

## Usage

``` r
is_code_chunk_header_or_xaringan_or_code_output(pd)
```

## Arguments

- pd:

  A parse table.

## Examples

``` r
style_text(c(
  "# title",
  "some_code <- function() {}",
  "#+ chunk-label, opt1=value1",
  "call(3, 2, c(3:2))",
  "#> 99"
))
#> # title
#> some_code <- function() {}
#> #+ chunk-label, opt1=value1
#> call(3, 2, c(3:2))
#> #> 99
```
