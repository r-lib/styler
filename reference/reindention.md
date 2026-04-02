# Specify what is re-indented how

This function returns a list that can be used as an input for the
argument `reindention` of the function
[`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md).
It features sensible defaults, so the user can specify deviations from
them conveniently without the need of setting all arguments explicitly.

## Usage

``` r
specify_reindention(regex_pattern = NULL, indention = 0L, comments_only = TRUE)

tidyverse_reindention()
```

## Arguments

- regex_pattern:

  Character vector with regular expression patterns that are to be
  re-indented with spaces, `NULL` if no reindention needed.

- indention:

  The indention tokens should have if they match `regex_pattern`.

- comments_only:

  Whether the `regex_reindention_pattern` should only be matched against
  comments or against all tokens. Mainly added for performance.

## Functions

- `specify_reindention()`: Allows to specify which tokens are reindented
  and how.

- `tidyverse_reindention()`: Simple forwarder to `specify_reindention`
  with reindention according to the tidyverse style guide.

## Examples

``` r
style_text("a <- xyz", reindention = specify_reindention(
  regex_pattern = "xyz", indention = 4, comments_only = FALSE
))
#> a <-    xyz
style_text("a <- xyz", reindention = tidyverse_reindention())
#> a <- xyz
```
