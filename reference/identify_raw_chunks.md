# Identifies raw Rmd or Rnw code chunks

Raw in the sense that these chunks don't contain pure R code, but they
contain a header and footer of markdown. Only code chunks that have an
engine whose name matches `engine-pattern` are considered as R code. For
every opening, we match the next closing. If there are not the same
amount of closing and openings after this matching, we throw an error.
Similarly, if there are two openings before a closing, the closing gets
matched twice, on which we throw an error.

## Usage

``` r
identify_raw_chunks(lines, filetype, engine_pattern = get_engine_pattern())
```

## Arguments

- lines:

  A character vector of lines from an Rmd or Rnw file.

- filetype:

  A string indicating the filetype - either 'Rmd' or 'Rnw'.

- engine_pattern:

  A regular expression that must match the engine name.
