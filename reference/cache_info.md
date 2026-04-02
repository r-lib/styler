# Show information about the styler cache

Gives information about the cache. Note that the size consumed by the
cache will always be displayed as zero because all the cache does is
creating an empty file of size 0 bytes for every cached expression. The
inode is excluded from this displayed size but negligible.

## Usage

``` r
cache_info(cache_name = NULL, format = "both")
```

## Arguments

- cache_name:

  The name of the cache for which to show details. If `NULL`, the active
  cache is used. If none is active the cache corresponding to the
  installed styler version is used.

- format:

  Either "lucid" for a summary emitted with
  [`base::cat()`](https://rdrr.io/r/base/cat.html), "tabular" for a
  tabular summary from
  [`base::file.info()`](https://rdrr.io/r/base/file.info.html) or "both"
  for both.

## See also

Other cache managers:
[`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md),
[`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md),
[`caching`](https://styler.r-lib.org/reference/caching.md)
