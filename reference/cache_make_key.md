# Make a key for `R.cache`

This is used to determine if caching already corresponds to a style
guide.

## Usage

``` r
cache_make_key(text, transformers, more_specs)
```

## Arguments

- text:

  Code to create a cache for. This should be styled text, as the
  approach used by styler does not cache input, but styled code.

- transformers:

  A list of transformer functions, because we can only know if text is
  already correct if we know which transformer function it should be
  styled with.

- more_specs:

  A named vector coercible to character that determines the styling but
  are style guide independent, such as `include_roxygen_examples` or
  `base_indention`.

## Details

We need to compare:

- text to style. Will be passed to hash function as is.

- styler version. Not an issue because for every version of styler, we
  build a new cache.

- transformers. Cannot easily hash them because two environments won't
  be identical even if they contain the same objects (see
  'Experiments'). Simple `as.character(transformers)` will not consider
  infinitely recursive code dependencies. To fix this, transformers must
  have names and version number as described in
  [`create_style_guide()`](https://styler.r-lib.org/reference/create_style_guide.md).
  Now, the only way to fool the cache invalidation is to replace a
  transformer with the same function body (but changing the function
  definition of the functions called in that body) interactively without
  changing version number of name at the same time. Remaining problem:
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  calls will render generic code, e.g. see
  `as.character(list(purrr::partial(sum, x = 4)))`. For that reason, all
  arguments passed to a
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  call must be put in the style guide under `more_specs_style_guide`.

## Experiments

There is unexplainable behavior in conjunction with hashing and
environments:

- Functions created with
  [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  are not identical when compared with
  [`identical()`](https://rdrr.io/r/base/identical.html)
  ([StackOverflow](https://stackoverflow.com/questions/58656033/when-are-purrrpartial-ized-functions-identical))

- except when they have the exact same parent environment, which must be
  an object created and then passed to `purrr::partial(.env = ...)`, not
  created in-place.

- [`purrr::partial()`](https://purrr.tidyverse.org/reference/partial.html)
  seems to ignore `.env` after version 0.2.5, so until this is fixed,
  we'd have to work with version 0.2.5.

- Our caching backend package, `R.cache`, uses
  `R.cache:::getChecksum.default` (which uses
  [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html))
  to hash the input. The latter does not seem to care if the
  environments are exactly equal (see 'Examples').

- However, under some circumstances, it does: Commit 9c94c022 (if not
  overwritten / rebased by now) contains a reprex. Otherwise, search for
  43219ixmypi in commit messages and restore this commit to reproduce
  the behavior.

## Examples

``` r
add <- function(x, y) {
  x + y
}
add1 <- purrr::partial(add, x = 1)
add2 <- purrr::partial(add, x = 1)
identical(add1, add2)
#> [1] TRUE
identical(digest::digest(add1), digest::digest(add2))
#> [1] TRUE
identical(digest::digest(styler::tidyverse_style()), digest::digest(styler::tidyverse_style()))
#> [1] TRUE
```
