# Specify which tokens must be absent for a transformer to be dropped

`{styler}` can remove transformer functions safely removed from the list
of transformers to be applied on every *nest* with
[`transformers_drop()`](https://styler.r-lib.org/reference/transformers_drop.md)
if the tokens that trigger a manipulation of the parse data are absent
in the text to style. `specify_transformers_drop()` helps you specify
these conditions.

## Usage

``` r
specify_transformers_drop(
  spaces = NULL,
  indention = NULL,
  line_breaks = NULL,
  tokens = NULL
)
```

## Arguments

- spaces, indention, line_breaks, tokens:

  Each a list (or `NULL`) where the name of each element is the
  concerning transformer, the value is an unnamed vector with tokens
  that match the rule. See 'Examples'.

## Details

Note that the negative formulation (must be absent in order to be
dropped) means that when you add a new rule and you forget to add a rule
for when to drop it, it will not be dropped. If we required to specify
the complement (which tokens must be present for the transformer to be
kept), the transformer would be silently removed, which is less save.

## Warning

It is the responsibility of the developer to ensure expected behavior,
in particular that:

- the name of the supplied dropping criteria matches the name of the
  transformer function.

- the dropping criteria (name + token) reflects correctly under which
  circumstances the transformer does not have an impact on styling and
  can therefore be safely removed without affecting the styling outcome.

You can use the unexported function
[`test_transformers_drop()`](https://styler.r-lib.org/reference/test_transformers_drop.md)
for some checks.

## Examples

``` r
dropping <- specify_transformers_drop(
  spaces = c(remove_space_after_excl = "'!'")
)
style_guide <- create_style_guide(
  space = list(remove_space_after_excl = styler:::remove_space_after_excl),
  transformers_drop = dropping
)
# transformers_drop() will remove the transformer when the code does not
# contain an exclamation mark
style_guide_with_some_transformers_dropped <- styler:::transformers_drop(
  "x <- 3;2", style_guide
)
setdiff(
  names(style_guide$space),
  names(style_guide_with_some_transformers_dropped)
)
#> [1] "remove_space_after_excl"
# note that dropping all transformers of a scope means that this scope
# has an empty named list for this scope
style_guide_with_some_transformers_dropped$space
#> named list()
# this is not the same as if this scope was never specified.
tidyverse_style(scope = "none")$space
#> NULL
# Hence, styler should check for length 0 to decide if a scope is present or
# not, not via `is.null()` and we can use the `is.null()` check to see if
# this scope was initially required by the user.
```
