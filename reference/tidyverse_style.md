# The tidyverse style

Style code according to the tidyverse style guide.

## Usage

``` r
tidyverse_style(
  scope = "tokens",
  strict = TRUE,
  indent_by = 2L,
  start_comments_with_one_space = FALSE,
  reindention = tidyverse_reindention(),
  math_token_spacing = tidyverse_math_token_spacing()
)
```

## Arguments

- scope:

  The extent of manipulation. Can range from "none" (least invasive) to
  "tokens" (most invasive). See 'Details'. This argument is a string or
  a vector of class `AsIs`.

- strict:

  A logical value indicating whether a set of strict or not so strict
  transformer functions should be returned. Compare the functions
  returned with or without `strict = TRUE`. For example, `strict = TRUE`
  means force *one* space e.g. after "," and *one* line break e.g. after
  a closing curly brace. `strict = FALSE` means to set spaces and line
  breaks to one if there is none and leave the code untouched otherwise.
  See 'Examples'.

- indent_by:

  How many spaces of indention should be inserted after operators such
  as '('.

- start_comments_with_one_space:

  Whether or not comments should start with only one space (see
  [`start_comments_with_space()`](https://styler.r-lib.org/reference/start_comments_with_space.md)).

- reindention:

  A list of parameters for regex re-indention, most conveniently
  constructed using
  [`specify_reindention()`](https://styler.r-lib.org/reference/reindention.md).

- math_token_spacing:

  A list of parameters that define spacing around math token,
  conveniently constructed using
  [`specify_math_token_spacing()`](https://styler.r-lib.org/reference/math_token_spacing.md).

## Details

The following levels for `scope` are available:

- "none": Performs no transformation at all.

- "spaces": Manipulates spacing between token on the same line.

- "indention": Manipulates the indention, i.e. number of spaces at the
  beginning of each line.

- "line_breaks": Manipulates line breaks between tokens.

- "tokens": manipulates tokens.

`scope` can be specified in two ways:

- As a string: In this case all less invasive scope levels are implied,
  e.g. "line_breaks" includes "indention", "spaces". This is brief and
  what most users need.

- As vector of class `AsIs`: Each level has to be listed explicitly by
  wrapping one ore more levels of the scope in
  [`I()`](https://rdrr.io/r/base/AsIs.html). This offers more granular
  control at the expense of more verbosity.

See 'Examples' for details.

## Examples

``` r
style_text("call( 1)", style = tidyverse_style, scope = "spaces")
#> call(1)
style_text("call( 1)", transformers = tidyverse_style(strict = TRUE))
#> call(1)
style_text(c("ab <- 3", "a  <-3"), strict = FALSE) # keeps alignment of "<-"
#> ab <- 3
#> a  <- 3
style_text(c("ab <- 3", "a  <-3"), strict = TRUE) # drops alignment of "<-"
#> ab <- 3
#> a <- 3

# styling line breaks only without spaces
style_text(c("ab <- 3", "a =3"), strict = TRUE, scope = I(c("line_breaks", "tokens")))
#> ab <- 3
#> a <-3
```
