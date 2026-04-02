# Activate or deactivate the styler cache

Helper functions to control the behavior of caching. Simple wrappers
around [`base::options()`](https://rdrr.io/r/base/options.html).

## Usage

``` r
cache_activate(cache_name = NULL, verbose = !getOption("styler.quiet", FALSE))

cache_deactivate(verbose = !getOption("styler.quiet", FALSE))
```

## Arguments

- cache_name:

  The name of the styler cache to use. If `NULL`, the option
  "styler.cache_name" is considered which defaults to the version of
  styler used.

- verbose:

  Whether or not to print an informative message about what the function
  is doing.

## See also

Other cache managers:
[`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md),
[`cache_info()`](https://styler.r-lib.org/reference/cache_info.md),
[`caching`](https://styler.r-lib.org/reference/caching.md)
