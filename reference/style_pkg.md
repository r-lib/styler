# Prettify R source code

Performs various substitutions in all `.R` files in a package (code and
tests), `.Rmd`, `.Rmarkdown` and/or `.qmd`, `.Rnw` files (vignettes and
readme). Carefully examine the results after running this function!

## Usage

``` r
style_pkg(
  pkg = ".",
  ...,
  style = tidyverse_style,
  transformers = style(...),
  filetype = c("R", "Rprofile", "Rmd", "Rmarkdown", "Rnw", "qmd"),
  exclude_files = c("R/RcppExports\\.R", "R/cpp11\\.R",
    "R/import-standalone.*\\.R"),
  exclude_dirs = c("packrat", "renv"),
  include_roxygen_examples = TRUE,
  base_indention = 0L,
  dry = "off"
)
```

## Arguments

- pkg:

  Path to a (subdirectory of an) R package.

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

- filetype:

  Vector of file extensions indicating which file types should be
  styled. Case is ignored, and the `.` is optional, e.g.
  `c(".R",".Rmd")`, or `c("r", "rmd")`. Supported values (after
  standardization) are: "qmd", "r", "rmd", "rmarkdown", "rnw", and
  "rprofile". Rmarkdown is treated as Rmd.

- exclude_files:

  Character vector with regular expressions to files that should be
  excluded from styling.

- exclude_dirs:

  Character vector with directories to exclude (recursively). Note that
  the default values were set for consistency with
  [`style_dir()`](https://styler.r-lib.org/reference/style_dir.md) and
  as these directories are anyways not styled.

- include_roxygen_examples:

  Whether or not to style code in roxygen examples.

- base_indention:

  Integer scalar indicating by how many spaces the whole output text
  should be indented. Note that this is not the same as splitting by
  line and add a `base_indention` spaces before the code in the case
  multi-line strings are present. See 'Examples'.

- dry:

  To indicate whether styler should run in *dry* mode, i.e. refrain from
  writing back to files .`"on"` and `"fail"` both don't write back, the
  latter returns an error if the input code is not identical to the
  result of styling. "off", the default, writes back if the input and
  output of styling are not identical.

## Warning

This function overwrites files (if styling results in a change of the
code to be formatted and `dry = "off"`). It is strongly suggested to
only style files that are under version control or to create a backup
copy.

We suggest to first style with `scope < "tokens"` and inspect and commit
changes, because these changes are guaranteed to leave the abstract
syntax tree (AST) unchanged. See section 'Round trip validation' for
details.

Then, we suggest to style with `scope = "tokens"` (if desired) and
carefully inspect the changes to make sure the AST is not changed in an
unexpected way that invalidates code.

## Round trip validation

The following section describes when and how styling is guaranteed to
yield correct code.

If tokens are not in the styling scope (as specified with the `scope`
argument), no tokens are changed and the abstract syntax tree (AST)
should not change. Hence, it is possible to validate the styling by
comparing whether the parsed expression before and after styling have
the same AST. This comparison omits roxygen code examples and comments.
styler throws an error if the AST has changed through styling.

Note that if tokens are to be styled, such a comparison is not conducted
because the AST might well change and such a change is intended. There
is no way styler can validate styling, that is why we inform the user to
carefully inspect the changes.

See section 'Warning' for a good strategy to apply styling safely.

## Value

Invisibly returns a data frame that indicates for each file considered
for styling whether or not it was actually changed (or would be changed
when `dry` is not "off").

## See also

Other stylers:
[`style_dir()`](https://styler.r-lib.org/reference/style_dir.md),
[`style_file()`](https://styler.r-lib.org/reference/style_file.md),
[`style_text()`](https://styler.r-lib.org/reference/style_text.md),
[`styler_addins`](https://styler.r-lib.org/reference/styler_addins.md)

## Examples

``` r
if (FALSE) {
# the following is identical (because of ... and defaults)
# but the first is most convenient:
style_pkg(strict = TRUE)
style_pkg(style = tidyverse_style, strict = TRUE)
style_pkg(transformers = tidyverse_style(strict = TRUE))

# more options from `tidyverse_style()`
style_pkg(
  scope = "line_breaks",
  math_token_spacing = specify_math_token_spacing(zero = "'+'")
)

# don't write back and fail if input is not already styled
style_pkg("/path/to/pkg/", dry = "fail")
}
```
