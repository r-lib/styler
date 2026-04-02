# Map the file type to a corresponding regular expression

Map the file type to a corresponding regular expression

## Usage

``` r
map_filetype_to_pattern(filetype)
```

## Arguments

- filetype:

  The file type to map to a regex.

## Examples

``` r
styler:::map_filetype_to_pattern(c(".rMd", "R"))
#> [1] "(\\.rmd|\\.r)$"
```
