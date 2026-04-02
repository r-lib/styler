# Transform mixed contents

Applies the supplied transformer function to code chunks identified
within an Rmd or Rnw file and recombines the resulting (styled) code
chunks with the text chunks.

## Usage

``` r
transform_mixed(lines, transformer_fun, filetype)
```

## Arguments

- lines:

  A character vector of lines from an Rmd or Rnw file.

- transformer_fun:

  A styler transformer function.

- filetype:

  A string indicating the filetype - either 'Rmd' or 'Rnw'.
