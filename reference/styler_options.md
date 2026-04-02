# Package options

These options can be set via
[`options()`](https://rdrr.io/r/base/options.html) and queried via
[`getOption()`](https://rdrr.io/r/base/options.html). For this, add a
`styler.` prefix (the package name and a dot) to the option name.
Example: for an option `foo`, use `options(styler.foo = value)` to set
it and `getOption("styler.foo")` to retrieve the current value. An
option value of `NULL` means that the default is used.

## Options for the styler package

- `styler.addins_style_transformer`: character. The name of the style
  transformer to use in the addins. (default:
  `"styler::tidyverse_style()"`)

- `styler.cache_name`: character. The name of the styler cache to use.
  (default: 1.11.0)

- `styler.cache_root`: character. The directory where the cache files
  are stored. For more, see
  [`help("caching")`](https://styler.r-lib.org/reference/caching.md).
  (default: `NULL`)

- `styler.colored_print.vertical`: logical. It decides whether or not
  the output should be colored with
  [`prettycode::highlight()`](https://rdrr.io/pkg/prettycode/man/highlight.html).
  (default: `TRUE`)

- `styler.ignore_alignment`: logical. If `TRUE`, alignment, when
  detected, is ignored. For more, see
  [`vignette("detect-alignment")`](https://styler.r-lib.org/articles/detect-alignment.md).
  (default: `FALSE`)

- `styler.ignore_start`, `styler.ignore_stop`: character. Regular
  expressions to ignore lines that match them. For more, see
  [`help("stylerignore")`](https://styler.r-lib.org/reference/stylerignore.md).
  (default: `"styler: off"` and `"styler: on"`, respectively)

- `styler.quiet`: logical. It decides whether or not to print an
  informative message about what the function is doing. If `TRUE`, no
  output is printed. (default: `FALSE`)

- `styler.test_dir_writable`: logical. If `TRUE`, the package tests
  whether the directory is writable. (default: `TRUE`)

## Examples

``` r
if (FALSE) {
getOption("styler.ignore_alignment")
options(
  styler.ignore_alignment = TRUE,
  styler.quiet = TRUE
)
getOption("styler.ignore_alignment")
}
```
