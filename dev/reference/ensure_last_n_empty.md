# Ensure there is one (and only one) blank line at the end of a vector

Ensure there is one (and only one) blank line at the end of a vector

## Usage

``` r
ensure_last_n_empty(x, n = 1L)
```

## Examples

``` r
styler:::ensure_last_n_empty("")
#> [1] ""
styler:::ensure_last_n_empty(letters)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z" "" 
styler:::ensure_last_n_empty(c(letters, "", "", ""))
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z" "" 
```
