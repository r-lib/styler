# Replace the newline character with a line break

Replace the newline character with a line break

## Usage

``` r
convert_newlines_to_linebreaks(text)
```

## Arguments

- text:

  A character vector

## Examples

``` r
styler:::convert_newlines_to_linebreaks("x\n2")
#> [1] "x" "2"
# a simple strsplit approach does not cover both cases
unlist(strsplit("x\n\n2", "\n", fixed = TRUE))
#> [1] "x" ""  "2"
unlist(strsplit(c("x", "", "2"), "\n", fixed = TRUE))
#> [1] "x" "2"
styler:::convert_newlines_to_linebreaks(c("x", "2"))
#> [1] "x" "2"
```
