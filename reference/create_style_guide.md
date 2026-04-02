# Create a style guide

This is a helper function to create a style guide, which is technically
speaking a named list of groups of transformer functions where each
transformer function corresponds to one styling rule. The output of this
function can be used as an argument for `style` in top-level functions
like [`style_text()`](https://styler.r-lib.org/reference/style_text.md)
and friends. Note that for caching to work properly, unquote all inputs
to the transformer function if possible with rlang's `!!`, otherwise,
they will be passed as references (generic variable names) instead of
literals and `styler:::is_cached()` won't pick up changes. See how it's
done in
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)
with `indent_by` and other arguments.

## Usage

``` r
create_style_guide(
  initialize = default_style_guide_attributes,
  line_break = NULL,
  space = NULL,
  token = NULL,
  indention = NULL,
  use_raw_indention = FALSE,
  reindention = tidyverse_reindention(),
  style_guide_name = NULL,
  style_guide_version = NULL,
  more_specs_style_guide = NULL,
  transformers_drop = specify_transformers_drop(),
  indent_character = " "
)
```

## Arguments

- initialize:

  The bare name of a function that initializes various variables on each
  level of nesting.

- line_break:

  A list of transformer functions that manipulate line_break
  information.

- space:

  A list of transformer functions that manipulate spacing information.

- token:

  A list of transformer functions that manipulate token text.

- indention:

  A list of transformer functions that manipulate indention.

- use_raw_indention:

  Boolean indicating whether or not the raw indention should be used.

- reindention:

  A list of parameters for regex re-indention, most conveniently
  constructed using
  [`specify_reindention()`](https://styler.r-lib.org/reference/reindention.md).

- style_guide_name:

  The name of the style guide. Used as a meta attribute inside the
  created style guide, for example for caching. By convention, this is
  the style guide qualified by the package namespace plus the location
  of the style guide, separated by `@`. For example,
  `"styler::tidyverse_style@https://github.com/r-lib"`.

- style_guide_version:

  The version of the style guide. Used as a meta attribute inside the
  created style guide, for example for caching. This should correspond
  to the version of the R package that exports the style guide.

- more_specs_style_guide:

  Named vector (coercible to character) with all arguments passed to the
  style guide and used for cache invalidation. You can easily capture
  them in your style guide function declaration with
  `as.list(environment())` (compare source code of
  [`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)).

- transformers_drop:

  A list specifying under which conditions transformer functions can be
  dropped since they have no effect on the code to format, most easily
  constructed with
  [`specify_transformers_drop()`](https://styler.r-lib.org/reference/specify_transformers_drop.md).
  This is argument experimental and may change in future releases
  without prior notification. It was mainly introduced to improve speed.
  Listing transformers here that occur almost always in code does not
  make sense because the process of excluding them also takes some time.

- indent_character:

  The character that is used for indention. We strongly advise for using
  spaces as indention characters.

## Examples

``` r
set_line_break_before_curly_opening <- function(pd_flat) {
  op <- pd_flat$token %in% "'{'"
  pd_flat$lag_newlines[op] <- 1L
  pd_flat
}
set_line_break_before_curly_opening_style <- function() {
  create_style_guide(
    line_break = list(set_line_break_before_curly_opening),
    style_guide_name = "some-style-guide",
    style_guide_version = "some-version"
  )
}
style_text(
  "a <- function(x) { x }",
  style = set_line_break_before_curly_opening_style
)
#> a <- function(x)
#> { x }
```
