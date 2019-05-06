# styler 1.1.1

This is primarily a maintenance release upon the request of the CRAN team
(#490).

## Major changes

- Users can now control style configurations for styler Addins (#463, #500),
  using the `Set style` Addin. See `?styler::styler_addins` for details.
- `%>%` almost always causes a line break now (#503) for `strict = TRUE`.

## Minor changes

- `style_pkg()` now also styles the "demo" directory by default (#453).

- multi-line strings are now styled more consistently (#459).

- indention in roxygen code example styling (#455) and EOF spacing (#469) was
  fixed.

- indention for for loop edge case (#457) and comments in pipe chain (#482)
  were fixed.

- line-break styling around comma is improved (#479).

-  bug that can cause an error when the variable `text` in any name space
  before styler on the search path was defined and did not have length 1 is
  fixed (#484).

- slightly confusing warning about empty strings caused with roxygen code
  examples and Rmd was removed.

- right apostrophe to let package pass R CMD Check in strict Latin-1
  locale was removed (#490, reason for release).

## Adaption of styler

Since it's never been mentioned in the release notes, we also mention here where
else you can use styler functionality:

* `usethis::use_tidy_style()` styles your project according to the tidyverse
  style guide.

* `reprex::reprex(style = TRUE)` to prettify reprex code before printing. To
  permanently use `style = TRUE` without specifying it every time, you can add
  the following line to your `.Rprofile` (via `usethis::edit_r_profile()`):
  `options(reprex.styler = TRUE)`.

* you can pretty-print your R code in RMarkdown reports without having styler
  modifying the source. This feature is implemented as a code chunk option in
  knitr. use `tidy = "styler"` in the header of a code chunks (e.g. ` ```{r
  name-of-the-chunk, tidy = "styler"}`), or `knitr::opts_chunk$set(tidy =
  "styler")` at the top of your RMarkdown script.

* pretty-printing of [drake](https://github.com/ropensci/drake) workflow data
  frames with `drake::drake_plan_source()`.

* Adding styler as a fixer to the [ale
  Plug-in](https://github.com/w0rp/ale/pull/2401#issuecomment-485942966) for
  VIM.

Thanks to all contributors involved, in particular
[&#x0040;ArthurPERE](https://github.com/ArthurPERE),
[&#x0040;hadley](https://github.com/hadley),
[&#x0040;igordot](https://github.com/igordot),
[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil),
[&#x0040;jackwasey](https://github.com/jackwasey),
[&#x0040;jcrodriguez1989](https://github.com/jcrodriguez1989),
[&#x0040;jennybc](https://github.com/jennybc),
[&#x0040;jonmcalder](https://github.com/jonmcalder),
[&#x0040;katrinleinweber](https://github.com/katrinleinweber),
[&#x0040;krlmlr](https://github.com/krlmlr),
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert),
[&#x0040;michaelquinn32](https://github.com/michaelquinn32),
[&#x0040;msberends](https://github.com/msberends),
[&#x0040;raynamharris](https://github.com/raynamharris),
[&#x0040;riccardoporreca](https://github.com/riccardoporreca),
[&#x0040;rjake](https://github.com/rjake),
[&#x0040;Robinlovelace](https://github.com/Robinlovelace),
[&#x0040;skirmer](https://github.com/skirmer),
[&#x0040;thalesmello](https://github.com/thalesmello),
[&#x0040;tobiasgerstenberg](https://github.com/tobiasgerstenberg),
[&#x0040;tvatter](https://github.com/tvatter),
[&#x0040;wdearden](https://github.com/wdearden),
[&#x0040;wmayner](https://github.com/wmayner), and
[&#x0040;yech1990](https://github.com/yech1990).

# styler 1.1.0

This release introduces new features and is fully backward-compatible. It also
adapts to changes in the R parser committed into R devel (#419).

## Major Changes

* styler can now style roxygen code examples in the source code of package
  (#332) as well as Rnw files (#431).

* the print method for the output of `style_text()` (`print.vertical()`) now
  returns syntax-highlighted code by default, controllable via the option
  `styler.colored_print.vertical` (#417).

* the README was redesigned (#413).

* semi-colon expression that contained multiple assignments was fixed (#404).

## Minor Changes

* cursor position is remembered for styling via Addin (#416).

* adapt spacing around tilde for multi-token expressions(#424) and brace edge
  case (#425).

* only add brackets to piped function call if RHS is a symbol (#422).

* increase coverage again to over 90% (#412).

* move rule that turns single quotes into double quotes to token modifier in
  `tidyverse_style_guide() (#406).

* remove line-breaks before commas (#405).

* removed package dependency enc in favor of xfun (#442).

Thanks to all contributors for patches, issues and the like: @jonmcalder,
@krlmlr, @IndrajeetPatil, @kalibera, @Hasnep, @kiranmaiganji, @dirkschumacher,
@ClaytonJY, @wlandau, @maurolepore

# styler 1.0.2

This is a maintenance release without any breaking API changes.

## Major Changes

* Fixed indention for named multi-line function calls (#372).

* Non-R code chunks in `.Rmd` files are now respected and won't get styled
  (#386).

## Minor Changes

* Fixing an edge case in which, if very long strings were present in the code,
  tokens could be replaced with wrong text (#384).

* Spacing around tilde in formulas depends now on whether there is a LHS in the
  formula (#379).

* Spaces are now also added around `EQ_SUB` (`=`) (#380).

* Added `CONTRIBUTING.md` to outline guidelines for contributing to styler.

* More informative error messages for parsing problems (#401, #400).

* Improved documentation (#387).

Thanks to all contributors for patches, issues and the like: @katrinleinweber,
@krlmlr, @dchiu911, @ramnathv, @aedobbyn, @Bio7, @tonytonov, @samhinshaw, @fny,
@vnijs, @martin-mfg, @NGaffney, @dchiu911.

# styler 1.0.1

This is a maintenance release without any breaking API changes.

## Major & dependency related changes

* Removed implicit `dplyr` dependency via `purrr:::map_dfr()` (thanks
  @jimhester, #324).

* Added required minimal version dependency for purr (`>= 0.2.3`) (#338).

* We rely on the tibble package which was optimized for speed in `v1.4.2` so
  styler should run ~2x as fast
  [(#348)](https://github.com/tidyverse/tibble/pull/348). For that reason,
  styler now depends on `tibble >= 1.4.2`.

* In the dependency `enc`, a bug was fixed that removed/changed non-ASCII
  characters. Hence, styler now depends on `enc >= 0.2` (#348).

## Minor changes

* We're now recognizing and respecting more DSLs used in R comments: rplumber
  (`#*`, #306), shebang `#/!` (#345), knitr chunk headers for spinning (`#+` /
  `#-`, #362).

* Named arguments can stay on the first line if call is multi-line (#318).

* No space anymore with `tidyverse_style()` after `!!` since with `rlang 0.2`,
  `!!` now binds tighter (#322), spacing around `~` (#316), no space anymore
  around `^` (#308).

* Code chunks in Rmd documents that don't use the R engine are no longer
  formatted (#313).

* Various bug fixes and edge case improvements.

Thanks to all contributors for patches, issues and the like: @devSJR, @klrmlr,
@yutannihilation, @samhinshaw, @martin-mfg, @jjramsey, @RMHogervorst, @wlandau,
@llrs, @aaronrudkin, @crew102, @jkgrain, @jennybc, @joranE.

# styler 1.0.0

Initial release.

## stylers
These are functions used to style code. They style a directory, a whole package,
a file or a string.
```
style_dir(path = ".", 
  ..., style = tidyverse_style, transformers = style(...), 
  filetype = "R", recursive = TRUE, exclude_files = NULL
)

style_pkg(pkg = ".", 
  ..., style = tidyverse_style, transformers = style(...), filetype = "R", 
  exclude_files = "R/RcppExports.R"
)


style_file(path, 
  ..., style = tidyverse_style, transformers = style(...)
)

style_text(text, ..., style = tidyverse_style, transformers = style(...))
```

## style guides
These functions are the style guides implemented.
```
tidyverse_style(
  scope = "tokens", 
  strict = TRUE, 
  indent_by = 2, 
  start_comments_with_one_space = FALSE, 
  reindention = tidyverse_reindention(), 
  math_token_spacing = tidyverse_math_token_spacing()
)
tidyverse_reindention()
tidyverse_math_token_spacing())
```

## style guide creators
This function is used to create a style guide.
```
create_style_guide(
  initialize = default_style_guide_attributes, 
  line_break = NULL, 
  space = NULL, 
  token = NULL, 
  indention = NULL, 
  use_raw_indention = FALSE, 
  reindention = tidyverse_reindention()
)
```

## Helpers
These are helper functions used to specify the style guides in use.

```
specify_math_token_spacing(
  zero = NULL, 
  one = c("'+'", "'-'", "'*'", "'/'", "'^'")
)

specify_reindention(
  regex_pattern = NULL, 
  indention = 0, 
  comments_only = TRUE
)
initialize_default_attributes(pd_flat)
```

