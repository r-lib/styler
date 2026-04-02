# Adds the stylerignore column

If a token should be ignored, the column is set to `TRUE`, otherwise to
`FALSE`.

## Usage

``` r
add_stylerignore(pd_flat)
```

## Arguments

- pd_flat:

  A parse table.

## Details

A token is ignored iff one of the two conditions hold:

- it falls between a start and a stop marker whereas the markers are on
  their own line. Which tokens are recognized as markers is controlled
  with the R options `styler.ignore_start` and `styler.ignore_stop`.

- it is not a comment, but the last token on the line is a marker.

See examples in
[stylerignore](https://styler.r-lib.org/reference/stylerignore.md). Note
that you should reuse the stylerignore column to compute switch points
or similar and not a plain
`pd$text %in% option_read("styler.ignore_start")` because that will fail
to give correct switch points in the case stylerignore sequences are
invalid.
