# Remove roxygen header

Can't simply remove the element with the regex because it may happen
that the roxygen tag is on the same line as its contents start.

## Usage

``` r
remove_roxygen_header(text)
```

## Examples

``` r
#' @examples c(1, 2)
```
