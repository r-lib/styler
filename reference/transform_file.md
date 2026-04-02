# Transform a file and output a customized message

Transforms file contents and outputs customized messages.

## Usage

``` r
transform_file(
  path,
  fun,
  max_char_path,
  message_before = "",
  message_after = " [DONE]",
  message_after_if_changed = " *",
  ...,
  dry
)
```

## Arguments

- path:

  A vector with file paths to transform.

- fun:

  A function that returns a character vector.

- max_char_path:

  The number of characters of the longest path. Determines the indention
  level of `message_after`.

- message_before:

  The message to print before the path.

- message_after:

  The message to print after the path.

- message_after_if_changed:

  The message to print after `message_after` if any file was
  transformed.

- ...:

  Further arguments passed to
  [`transform_utf8()`](https://styler.r-lib.org/reference/transform_utf8.md).

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.
