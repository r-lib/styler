# Add information about previous / next token to each terminal

Note that this does function must be called in
[`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md)
and we cannot wait to initialize this attribute until
[`apply_transformers()`](https://styler.r-lib.org/reference/apply_transformers.md),
where all other attributes are initialized with
[`default_style_guide_attributes()`](https://styler.r-lib.org/reference/default_style_guide_attributes.md)
(when using
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md))
because for cached code, we don't build up the nested structure and
leave it shallow (to speed up things), see also
[`shallowify()`](https://styler.r-lib.org/reference/shallowify.md).

## Usage

``` r
add_terminal_token_after(pd_flat)

add_terminal_token_before(pd_flat)

add_attributes_caching(pd_flat, transformers, more_specs)
```

## Arguments

- pd_flat:

  A flat parse table.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- more_specs:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

## Functions

- `add_attributes_caching()`: Initializes `newlines` and `lag_newlines`.
