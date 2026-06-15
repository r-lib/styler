# Fix [`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html) output

Since [`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html)
treats braces in quotes as literal braces when determining brace
symmetry, a brace might be added in error to the parsed data (at the
end). We'll remove one at the time, check if output is parsable until no
braces are left. If we end up with no braces left, we signal a parsing
error, otherwise, we return the initial (not parsable input due to
*dont* sequence) with the trailing braces removed.

## Usage

``` r
roxygen_remove_extra_brace(parsed)
```

## Examples

``` r
styler:::parse_roxygen(
  c(
    "#' @examples",
    "#' x <- '{'",
    "#' \\dontrun{",
    "#' fu(x = 3)",
    "#' }"
  )
)
#> $text
#>  [1] "\n"          "x <- '"      ""            "{"           "'\n"        
#>  [6] "\\dontrun"   "{"           "\n"          "fu(x = 3)\n" "}"          
#> [11] "\n"         
#> 
#> $example_type
#> [1] "examples"
#> 
styler:::parse_roxygen(
  c(
    "#' @examples",
    "#' x <- '{'",
    "#' \\dontrun{",
    "#' c('{', \"'{{{\" ,\"[\")",
    "#' }"
  )
)
#> $text
#>  [1] "\n"                        "x <- '"                   
#>  [3] ""                          "{"                        
#>  [5] "'\n"                       "\\dontrun"                
#>  [7] "{"                         "\n"                       
#>  [9] "c('{', \"'{{{\" ,\"[\")\n" "}\n"                      
#> 
#> $example_type
#> [1] "examples"
#> 
```
