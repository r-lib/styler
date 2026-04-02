# Standardize text for hashing

Make sure text after styling results in the same hash as text before
styling if it is indeed identical. This function expects trailing blank
lines in `text` were removed prior to passing it to this function.

## Usage

``` r
hash_standardize(text)
```

## Arguments

- text:

  A character vector.
