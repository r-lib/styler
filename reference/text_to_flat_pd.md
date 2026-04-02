# Creates a flat parse table with minimal initialization

Creates a flat parse table with minimal initialization and makes the
parse table shallow where appropriate.

## Usage

``` r
text_to_flat_pd(text, transformers, more_specs)
```

## Arguments

- text:

  The text to parse.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- more_specs:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

## Details

This includes:

- token before and after.

- stylerignore attribute.

- caching attributes.

Note that the parse table might be shallow if caching is enabled and
some values are cached.
