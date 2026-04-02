# Stylers for RStudio Addins

Helper functions for styling via RStudio Addins.

## Addins

- Set style: Select the style transformers to use. For flexibility, the
  user input is passed to the `transformers` argument, not the `style`
  argument, so entering `styler::tidyverse_style(scope = "spaces")` in
  the Addin is equivalent to
  `styler::style_text("1+1", scope = "spaces")` and
  `styler::style_text("1+1", transformers = styler::tidyverse_style(scope = "spaces"))`
  if the text to style is `1+1`. The style transformers are memorized
  within an R session via the R option `styler.addins_style_transformer`
  so if you want it to persist over sessions, set the option
  `styler.addins_style_transformer` in your `.Rprofile`.

- Style active file: Styles the active file, by default with
  [`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)
  or the value of the option `styler.addins_style_transformer` if
  specified.

- Style selection: Same as *Style active file*, but styles the
  highlighted code instead of the whole file.

## Auto-Save Option

By default, both of the RStudio Addins will apply styling to the
(selected) file contents without saving changes. Automatic saving can be
enabled by setting the R option `styler.save_after_styling` to `TRUE`.
Consider setting this in your `.Rprofile` file if you want to persist
this setting across multiple sessions. Untitled files will always need
to be saved manually after styling.

## Life cycle

The way of specifying the style in the Addin as well as the auto-save
option (see below) are experimental. We are currently considering
letting the user specify the defaults for other style APIs like
[`style_text()`](https://styler.r-lib.org/reference/style_text.md),
either via R options, config files or other ways as well. See
[r-lib/styler#319](https://github.com/r-lib/styler/issues/319) for the
current status of this.

## See also

Other stylers:
[`style_dir()`](https://styler.r-lib.org/reference/style_dir.md),
[`style_file()`](https://styler.r-lib.org/reference/style_file.md),
[`style_pkg()`](https://styler.r-lib.org/reference/style_pkg.md),
[`style_text()`](https://styler.r-lib.org/reference/style_text.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# save after styling when using the Addin
options(styler.save_after_styling = TRUE)
# only style with scope = "spaces" when using the Addin
val <- "styler::tidyverse_style(scope = 'spaces')"
options(
  styler.addins_style_transformer = val
)
} # }
```
