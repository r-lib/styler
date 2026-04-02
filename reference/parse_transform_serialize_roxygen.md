# Parse, transform and serialize roxygen comments

Splits `text` into roxygen code examples and non-roxygen code examples
and then maps over these examples by applying
[`style_roxygen_code_example()`](https://styler.r-lib.org/reference/style_roxygen_code_example.md).

## Usage

``` r
parse_transform_serialize_roxygen(text, transformers, base_indention)
```

## Hierarchy

Styling involves splitting roxygen example code into segments, and
segments into snippets. This describes the process for input of
`parse_transform_serialize_roxygen()`:

- Splitting code into roxygen example code and other code. Downstream,
  we are only concerned about roxygen code. See
  `parse_transform_serialize_roxygen()`.

- Every roxygen example code can have zero or more dontrun / dontshow /
  donttest sequences. We next create segments of roxygen code examples
  that contain at most one of these. See
  [`style_roxygen_code_example()`](https://styler.r-lib.org/reference/style_roxygen_code_example.md).

- We further split the segment that contains at most one dont\* sequence
  into snippets that are either don\* or not. See
  [`style_roxygen_code_example_segment()`](https://styler.r-lib.org/reference/style_roxygen_code_example_segment.md).

Finally, that we have roxygen code snippets that are either dont\* or
not, we style them in
[`style_roxygen_example_snippet()`](https://styler.r-lib.org/reference/style_roxygen_example_snippet.md)
using
[`parse_transform_serialize_r()`](https://styler.r-lib.org/reference/parse_transform_serialize_r.md).
