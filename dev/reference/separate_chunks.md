# Separate chunks within Rmd and Rnw contents

Identifies and separates the code and text chunks (the latter includes
non-R code) within an Rmd or Rnw file, and returns these separately.

## Usage

``` r
separate_chunks(lines, filetype)
```

## Arguments

- lines:

  A character vector of lines from an Rmd or Rnw file.

- filetype:

  A string indicating the filetype - either 'Rmd' or 'Rnw'.
