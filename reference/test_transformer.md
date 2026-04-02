# Transforming test input with a transformer function

These functions can be used as inputs for
[`test_collection()`](https://styler.r-lib.org/reference/test_collection.md)
and
[`transform_and_check()`](https://styler.r-lib.org/reference/transform_and_check.md).

## Usage

``` r
style_empty(text, base_indention = 0L)

style_op(text, base_indention = 0L)
```

## Arguments

- text:

  A character vector to transform.

## Details

As inputs for
[`test_collection()`](https://styler.r-lib.org/reference/test_collection.md),
we can also use top-level functions such as
[`style_text()`](https://styler.r-lib.org/reference/style_text.md).

## Functions

- `style_empty()`: Nest and unnest `text` without applying any
  transformations but remove EOL spaces and indention due to the way the
  serialization is set up.

- `style_op()`: Transformations for indention based on operators
