# Closure to return a transformer function

This function takes a list of transformer functions as input and returns
a function that can be applied to character strings that should be
transformed.

## Usage

``` r
make_transformer(
  transformers,
  include_roxygen_examples,
  base_indention,
  warn_empty = TRUE
)
```

## Arguments

- transformers:

  A list of transformer functions that operate on flat parse tables.

- include_roxygen_examples:

  Whether or not to style code in roxygen examples.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

- warn_empty:

  Whether or not a warning should be displayed when `text` does not
  contain any tokens.
