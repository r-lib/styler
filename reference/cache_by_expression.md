# Cache text

Splits `text` into expressions and adds these to the cache. Note that
top-level comments are **not** cached because caching and in particular
checking if they are cached is too expensive. Comments may be cached as
part of the whole text (as opposed to on an expression by expression
basis) using
[`cache_write()`](https://styler.r-lib.org/reference/cache_write.md)
directly. Also, we must not cache stylerignore sequence, because we
might see the same expression that does not comply with the style guide
outside a stylerignore sequence and wrongly think we should leave it as
is.

## Usage

``` r
cache_by_expression(text, transformers, more_specs)
```

## Arguments

- text:

  A character vector with one or more expressions.

- transformers:

  A list of transformer functions, because we can only know if text is
  already correct if we know which transformer function it should be
  styled with.

- more_specs:

  A named vector coercible to character that determines the styling but
  are style guide independent, such as `include_roxygen_examples` or
  `base_indention`.
