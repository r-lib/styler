# Obtain robust parse data

Wrapper around `utils::getParseData(parse(text = text))` that returns a
flat parse table. When caching information should be added, make sure
that the cache is activated with
[`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md)
and both `transformers` and `cache_dir` are non-`NULL`.

## Usage

``` r
get_parse_data(text, include_text = TRUE, ...)
```

## Arguments

- text:

  The text to parse.

- include_text:

  Passed to
  [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)
  as `includeText`.

- ...:

  Other arguments passed to
  [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html).
