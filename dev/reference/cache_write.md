# Write to the cache

Write to the cache

## Usage

``` r
cache_write(text, transformers, more_specs)
```

## Arguments

- text:

  Code to create a cache for. This should be styled text, as the
  approach used by styler does not cache input, but styled code.

- transformers:

  A list of transformer functions, because we can only know if text is
  already correct if we know which transformer function it should be
  styled with.

- more_specs:

  A named vector coercible to character that determines the styling but
  are style guide independent, such as `include_roxygen_examples` or
  `base_indention`.
