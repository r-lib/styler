# Set the file type argument

Sets and asserts the file type argument to a standard format for further
internal processing.

## Usage

``` r
set_and_assert_arg_filetype(filetype)
```

## Arguments

- filetype:

  A character vector with file types to convert to the internal standard
  format.

## Examples

``` r
styler:::set_and_assert_arg_filetype("rMd")
#> [1] "\\.rmd"
try(styler:::set_and_assert_arg_filetype("xyz"))
#> Error in assert_filetype(without_dot) : 
#>   filetype must not contain other values than 'qmd', 'R', 'Rmarkdown', 'Rmd', 'Rnw', or 'Rprofile' (case is ignored).
```
