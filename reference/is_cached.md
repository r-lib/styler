# Check if text is cached

This boils down to check if the hash exists at the caching dir as a
file.

## Usage

``` r
is_cached(text, transformers, more_specs, cache_dir = get_cache_dir())
```

## Arguments

- text:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- more_specs:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- cache_dir:

  The caching directory relative to the `.Rcache` root to look for a
  cached value.
