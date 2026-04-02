# Package index

## Styling API

Functions for styling code

- [`style_text()`](https://styler.r-lib.org/reference/style_text.md) :
  Style a string
- [`style_file()`](https://styler.r-lib.org/reference/style_file.md) :
  Style files with R source code
- [`style_pkg()`](https://styler.r-lib.org/reference/style_pkg.md) :
  Prettify R source code
- [`style_dir()`](https://styler.r-lib.org/reference/style_dir.md) :
  Prettify arbitrary R code
- [`styler_addins`](https://styler.r-lib.org/reference/styler_addins.md)
  : Stylers for RStudio Addins
- [`styler_options`](https://styler.r-lib.org/reference/styler_options.md)
  : Package options

## Fine-tune styling

Customize style guides

- [`tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.md)
  : The tidyverse style
- [`specify_reindention()`](https://styler.r-lib.org/reference/reindention.md)
  [`tidyverse_reindention()`](https://styler.r-lib.org/reference/reindention.md)
  : Specify what is re-indented how
- [`specify_math_token_spacing()`](https://styler.r-lib.org/reference/math_token_spacing.md)
  [`tidyverse_math_token_spacing()`](https://styler.r-lib.org/reference/math_token_spacing.md)
  : Specify spacing around math tokens
- [`create_style_guide()`](https://styler.r-lib.org/reference/create_style_guide.md)
  : Create a style guide
- [`specify_transformers_drop()`](https://styler.r-lib.org/reference/specify_transformers_drop.md)
  : Specify which tokens must be absent for a transformer to be dropped

## Non-functional documentation

Explaining features

- [`caching`](https://styler.r-lib.org/reference/caching.md) : Remember
  the past to be quicker in the future
- [`stylerignore`](https://styler.r-lib.org/reference/stylerignore.md) :
  Turn off styling for parts of the code
- [`styler`](https://styler.r-lib.org/reference/styler-package.md)
  [`styler-package`](https://styler.r-lib.org/reference/styler-package.md)
  : styler: Non-Invasive Pretty Printing of R Code

## Caching

Utilities to help manage the styler cache

- [`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md)
  [`cache_deactivate()`](https://styler.r-lib.org/reference/cache_activate.md)
  : Activate or deactivate the styler cache
- [`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md) :
  Clear the cache
- [`cache_info()`](https://styler.r-lib.org/reference/cache_info.md) :
  Show information about the styler cache

## Third-party style guide helpers

Utilities for customizing styler for non-tidyverse style guides

- [`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md)
  : Obtain a nested parse table from a character vector
- [`next_non_comment()`](https://styler.r-lib.org/reference/next_non_comment.md)
  [`previous_non_comment()`](https://styler.r-lib.org/reference/next_non_comment.md)
  : Find the index of the next or previous non-comment in a parse table.
- [`is_curly_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_for_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_conditional_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_while_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_function_call()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_function_declaration()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_comment()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_tilde_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_asymmetric_tilde_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  [`is_symmetric_tilde_expr()`](https://styler.r-lib.org/reference/pd_is.md)
  : What is a parse table representing?
- [`scope_normalize()`](https://styler.r-lib.org/reference/scope_normalize.md)
  : Convert the styling scope to its lower-level representation

## Other

- [`print(`*`<vertical>`*`)`](https://styler.r-lib.org/reference/print.vertical.md)
  : Print styled code
