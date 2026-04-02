# Drop start / stop, when formatting is turned off

If `tidy = FALSE` (the knitr code chunk default), code is not styled
upon knitting. If it is explicitly added to a code chunk, the code chunk
is in addition not styled with styler when formatting the document.

## Usage

``` r
finalize_raw_chunks(start, end, filetype, lines)
```
