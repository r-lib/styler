# Transform a file an check the result

Transform an file and check whether it is identical to a reference.

## Usage

``` r
transform_and_check(
  in_item,
  out_item,
  in_name = in_item,
  out_name = out_item,
  transformer,
  dry,
  write_tree = FALSE,
  out_tree = "_tree",
  ...
)
```

## Arguments

- in_item:

  An path to an file to transform.

- out_item:

  The path to a file that contains the expected result.

- in_name:

  The label of the in_item, defaults to `in_item`.

- out_name:

  The label of the out_item, defaults to `out_item`.

- transformer:

  A function to apply to the content of `in_item`.

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.

- write_tree:

  Whether or not the tree structure of the test should be computed and
  written to a files.

- out_tree:

  Name of tree file if written out.

- ...:

  Parameters passed to transformer function.
