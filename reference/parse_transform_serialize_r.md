# Parse, transform and serialize text

Wrapper function for the common three operations.

## Usage

``` r
parse_transform_serialize_r(
  text,
  transformers,
  base_indention,
  warn_empty = TRUE,
  is_roxygen_code_example = FALSE
)
```

## Arguments

- text:

  The text to parse.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

- warn_empty:

  Whether or not a warning should be displayed when `text` does not
  contain any tokens.

- is_roxygen_code_example:

  Is code a roxygen examples block?

## See also

[`parse_transform_serialize_roxygen()`](https://styler.r-lib.org/reference/parse_transform_serialize_roxygen.md)
