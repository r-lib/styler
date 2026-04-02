# Construct \*-out.R from a \*-in.R

Multiple \*-in.R files can have the same \*-out.R file since to create
the \*-out.R file, everything after the first dash is replaced by
\*-out.R.

## Usage

``` r
construct_out(in_paths)
```

## Arguments

- in_paths:

  A character vector that denotes paths to \*-in.R files.

## Examples

``` r
styler:::construct_out(c(
  "path/to/file/first-in.R",
  "path/to/file/first-extended-in.R"
))
#> [1] "path/to/file/first-out.R"          "path/to/file/first-extended-out.R"
```
