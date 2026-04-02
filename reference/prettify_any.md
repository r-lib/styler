# Prettify R code in current working directory

This is a helper function for style_dir.

## Usage

``` r
prettify_any(
  transformers,
  filetype,
  recursive,
  exclude_files,
  exclude_dirs,
  include_roxygen_examples,
  base_indention = 0L,
  dry
)
```

## Arguments

- transformers:

  A set of transformer functions. This argument is most conveniently
  constructed via the `style` argument and `...`. See 'Examples'.

- filetype:

  Vector of file extensions indicating which file types should be
  styled. Case is ignored, and the `.` is optional, e.g.
  `c(".R",".Rmd")`, or `c("r", "rmd")`. Supported values (after
  standardization) are: "qmd", "r", "rmd", "rmarkdown", "rnw", and
  "rprofile". Rmarkdown is treated as Rmd.

- recursive:

  A logical value indicating whether or not files in subdirectories
  should be styled as well.

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
