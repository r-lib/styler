# Given a code snippet is dont\* or run, style it

Given a code snippet is dont\* or run, style it

## Usage

``` r
style_roxygen_example_snippet(
  code_snippet,
  transformers,
  is_dont,
  base_indention
)
```

## Arguments

- code_snippet:

  A character vector with code to style.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

- is_dont:

  Whether the snippet to process is a dontrun, dontshow, donttest
  segment or not.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

## Hierarchy

Styling involves splitting roxygen example code into segments, and
segments into snippets. This describes the process for input of
[`parse_transform_serialize_roxygen()`](https://styler.r-lib.org/reference/parse_transform_serialize_roxygen.md):

- Splitting code into roxygen example code and other code. Downstream,
  we are only concerned about roxygen code. See
  [`parse_transform_serialize_roxygen()`](https://styler.r-lib.org/reference/parse_transform_serialize_roxygen.md).

- Every roxygen example code can have zero or more dontrun / dontshow /
  donttest sequences. We next create segments of roxygen code examples
  that contain at most one of these. See
  [`style_roxygen_code_example()`](https://styler.r-lib.org/reference/style_roxygen_code_example.md).

- We further split the segment that contains at most one dont\* sequence
  into snippets that are either don\* or not. See
  [`style_roxygen_code_example_segment()`](https://styler.r-lib.org/reference/style_roxygen_code_example_segment.md).

Finally, that we have roxygen code snippets that are either dont\* or
not, we style them in `style_roxygen_example_snippet()` using
[`parse_transform_serialize_r()`](https://styler.r-lib.org/reference/parse_transform_serialize_r.md).
