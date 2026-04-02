# Style a roxygen code example with exactly one `@example` or `@exampleIf`

Style a roxygen code example with exactly one `@example` or `@exampleIf`

## Usage

``` r
style_roxygen_code_example_one(example_one, transformers, base_indention)
```

## Arguments

- example_one:

  A character vector, one element per line, that contains in total at
  most one example tag.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.
