# Style a roxygen code example segment

A roxygen code example segment corresponds to roxygen example code that
contains at most one `\\dontrun{...}` or friends. We drop all newline
characters first because otherwise the code segment passed to this
function was previously parsed with
[`parse_roxygen()`](https://styler.r-lib.org/reference/parse_roxygen.md)
and line-breaks in and after the `\\dontrun{...}` are expressed with
`"\n"`, which contradicts to the definition used elsewhere in this
package, where every element in a vector corresponds to a line. These
line-breaks don't get eliminated because they move to the front of a
`code_segment` and `style_text("\n1")` gives `"\n1"`, i.e. trailing
newlines are not eliminated.

## Usage

``` r
style_roxygen_code_example_segment(one_dont, transformers, base_indention)
```

## Arguments

- one_dont:

  Bare R code containing at most one `\\dontrun{...}` or friends.

- transformers:

  Passed to
  [`cache_make_key()`](https://styler.r-lib.org/reference/cache_make_key.md)
  to generate a key.

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
  `style_roxygen_code_example_segment()`.

Finally, that we have roxygen code snippets that are either dont\* or
not, we style them in
[`style_roxygen_example_snippet()`](https://styler.r-lib.org/reference/style_roxygen_example_snippet.md)
using
[`parse_transform_serialize_r()`](https://styler.r-lib.org/reference/parse_transform_serialize_r.md).
