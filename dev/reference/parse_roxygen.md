# Parse roxygen comments into text

Used to parse roxygen code examples. Removes line break before
`\\dontrun{...}` and friends because it does not occur for segments
other than `\\dont{...}` and friends.

## Usage

``` r
parse_roxygen(roxygen)
```

## Arguments

- roxygen:

  Roxygen comments.

## Examples

``` r
styler:::parse_roxygen(c(
  "#' @examples",
  "#' 1+  1"
))
#> $text
#> [1] "\n"      "1+  1\n"
#> 
#> $example_type
#> [1] "examples"
#> 
styler:::parse_roxygen(c(
  "#' @examples 33",
  "#'1+  1"
))
#> $text
#> [1] "33\n"    "1+  1\n"
#> 
#> $example_type
#> [1] "examples"
#> 
```
