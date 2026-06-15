# Transform files with transformer functions

`transform_files` applies transformations to file contents and writes
back the result.

## Usage

``` r
transform_files(
  files,
  transformers,
  include_roxygen_examples,
  base_indention,
  dry
)
```

## Arguments

- files:

  A character vector with paths to the file that should be transformed.

- transformers:

  A list of transformer functions that operate on flat parse tables.

- include_roxygen_examples:

  Whether or not to style code in roxygen examples.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.

## Value

Invisibly returns a data frame that indicates for each file considered
for styling whether or not it was actually changed (or would be changed
when `dry` is not "off").
