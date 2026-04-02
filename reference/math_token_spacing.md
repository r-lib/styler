# Specify spacing around math tokens

Helper function to create the input for the argument
`math_token_spacing` in
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md).

## Usage

``` r
specify_math_token_spacing(zero = "'^'", one = c("'+'", "'-'", "'*'", "'/'"))

tidyverse_math_token_spacing()
```

## Arguments

- zero:

  Character vector of tokens that should be surrounded with zero spaces.

- one:

  Character vector with tokens that should be surrounded by at least one
  space (depending on `strict = TRUE` in the styling functions
  [`style_text()`](https://styler.r-lib.org/reference/style_text.md) and
  friends). See 'Examples'.

## Functions

- `specify_math_token_spacing()`: Allows to fully specify the math token
  spacing.

- `tidyverse_math_token_spacing()`: Simple forwarder to
  `specify_math_token_spacing` with spacing around math tokens according
  to the tidyverse style guide.

## Examples

``` r
style_text(
  "1+1   -3",
  math_token_spacing = specify_math_token_spacing(zero = "'+'"),
  strict = FALSE
)
#> 1+1   - 3
style_text(
  "1+1   -3",
  math_token_spacing = specify_math_token_spacing(zero = "'+'"),
  strict = TRUE
)
#> 1+1 - 3
style_text(
  "1+1   -3",
  math_token_spacing = tidyverse_math_token_spacing(),
  strict = FALSE
)
#> 1 + 1   - 3
style_text(
  "1+1   -3",
  math_token_spacing = tidyverse_math_token_spacing(),
  strict = TRUE
)
#> 1 + 1 - 3
```
