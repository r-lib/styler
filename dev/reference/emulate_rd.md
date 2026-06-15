# Convert roxygen comments to Rd code

We leverage roxygen2 workhorse function
[`roxygen2::roc_proc_text()`](https://roxygen2.r-lib.org/reference/roc_proc_text.html)
if our input contains character that have to be escaped. Since this is
an expensive operation, we opt out of it and perform a simple
`remove_roxygen_mask()` when there are no characters to escape.

## Usage

``` r
emulate_rd(roxygen)
```
