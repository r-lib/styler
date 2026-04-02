# Style a string

Styles a character vector. Each element of the character vector
corresponds to one line of code.

## Usage

``` r
style_text(
  text,
  ...,
  style = tidyverse_style,
  transformers = style(...),
  include_roxygen_examples = TRUE,
  base_indention = 0L
)
```

## Arguments

- text:

  A character vector with text to style.

- ...:

  Arguments passed on to the `style` function, see
  [`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)
  for the default argument.

- style:

  A function that creates a style guide to use, by default
  [`tidyverse_style`](https://styler.r-lib.org/reference/tidyverse_style.md).
  Not used further except to construct the argument `transformers`. See
  [`style_guides()`](https://styler.r-lib.org/reference/style_guides.md)
  for details.

- transformers:

  A set of transformer functions. This argument is most conveniently
  constructed via the `style` argument and `...`. See 'Examples'.

- include_roxygen_examples:

  Whether or not to style code in roxygen examples.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

## See also

Other stylers:
[`style_dir()`](https://styler.r-lib.org/reference/style_dir.md),
[`style_file()`](https://styler.r-lib.org/reference/style_file.md),
[`style_pkg()`](https://styler.r-lib.org/reference/style_pkg.md),
[`styler_addins`](https://styler.r-lib.org/reference/styler_addins.md)

## Examples

``` r
style_text("call( 1)")
#> call(1)
style_text("1    + 1", strict = FALSE)
#> 1    + 1

# the following is identical (because of ... and defaults)
# but the first is most convenient:
style_text("a<-3++1", strict = TRUE)
#> a <- 3 + +1
style_text("a<-3++1", style = tidyverse_style, strict = TRUE)
#> a <- 3 + +1
style_text("a<-3++1", transformers = tidyverse_style(strict = TRUE))
#> a <- 3 + +1

# more invasive scopes include less invasive scopes by default
style_text("a%>%b", scope = "spaces")
#> a %>% b
style_text("a%>%b; a", scope = "line_breaks")
#> a %>% b; a
style_text("a%>%b; a", scope = "tokens")
#> a %>% b()
#> a

# opt out with I() to only style specific levels
style_text("a%>%b; a", scope = I("tokens"))
#> a%>%b()
#> a
```
