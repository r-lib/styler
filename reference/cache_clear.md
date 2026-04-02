# Clear the cache

Clears the cache that stores which files are already styled. You won't
be able to undo this. Note that the file corresponding to the cache (a
folder on your file system) won't be deleted, but it will be empty after
calling `cache_clear`.

## Usage

``` r
cache_clear(cache_name = NULL, ask = TRUE)
```

## Arguments

- cache_name:

  The name of the styler cache to use. If `NULL`, the option
  "styler.cache_name" is considered which defaults to the version of
  styler used.

- ask:

  Whether or not to interactively ask the user again.

## Details

Each version of styler has its own cache by default, because styling is
potentially different with different versions of styler.

## See also

Other cache managers:
[`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md),
[`cache_info()`](https://styler.r-lib.org/reference/cache_info.md),
[`caching`](https://styler.r-lib.org/reference/caching.md)
