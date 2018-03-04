## styler 1.0.1 (2018-03-02)

This is a maintenance release without any breaking API changes.

## Major & dependency related changes

* Removed implicit `dplyr` dependency via `purrr:::map_dfr()` (thanks 
  @jimhester, #324).
* Added required minimal version dependency for purr (`>= 0.2.3`) (#338).
* The dependency tibble was optimized for speed in `v1.4.2` so styler should run 
  ~2x as fast [(#348)](https://github.com/tidyverse/tibble/pull/348). Hence, 
  styler now depends on `tibble >= 1.4.2`.
* In the dependency `enc`, a bug was fixed that removed/changed non-ASCII 
  characters. Hence, styler now depends on `enc >= 0.1-10` (#348).

## Minor changes

* Recognizing and respecting of DSLs used in R comments: rplumnber (`#*`, #306), 
  shebang `#/!` (#345), knitr chunk headers for spinning (`#+` / `#-`, #362).
* Named arguments can stay on the first line if call is multi-line (#318).
* No space anymore with `tidyverse_style()` after `!!` since with `rlang 0.2`, 
  `!!` now binds tighter (#322), spacing around `~` (#316), no space anymore 
  around `^` (#308).
* Various bug fixes and edge case improvements.

Thanks to all contributors for patches, issues and the like: 
@devSJR, @klrmlr, @yutannihilation, @samhinshaw, @martin-mfg, @jjramsey, 
@RMHogervorst, @wlandau, @llrs, @aaronrudkin, @crew102, @jkgrain, @jennybc, 
@joranE.

## styler 1.0.0 (2017-12-05)

Initial release.

### stylers
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

### style guides
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

### style guide creators
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

### Helpers
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
