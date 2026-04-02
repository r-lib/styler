# Remember the past to be quicker in the future

Caching makes styler faster on repeated styling and is shared across all
APIs (e.g.
[`style_text()`](https://styler.r-lib.org/reference/style_text.md) and
Addin). That means if you style code that already complies to a style
guide and you have previously styled that code, it will be quicker.

## Configuring the cache

To comply with the CRAN policy, {styler} will by default clean up cache
files that are older than 6 days. This implies that you loose the
benefit of the cache for the files not styled in the last 6 days.

If you want to avoid this, i.e., if you want the cache to last longer,
you can use the R option `styler.cache_root` to opt for an indefinitely
long-lived cache by setting it to
`options(styler.cache_root = "styler-perm")`.

If you are happy with the cache being cleared after 6 days, you can
confirm the default and silence this message by setting it instead to
`options(styler.cache_root = "styler")`.

You can make this change in your `.Rprofile` using
`usethis::edit_r_profile()`.

## Manage the cache

See
[`cache_info()`](https://styler.r-lib.org/reference/cache_info.md),[`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md)
or [`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md)
for utilities to manage the cache. You can deactivate it altogether with
[`cache_deactivate()`](https://styler.r-lib.org/reference/cache_activate.md).
Since we leverage `{R.cache}` to manage the cache, you can also use any
`{R.cache}` functionality to manipulate it.

In some cases, you want to use a non-standard cache location. In that
situation, you can set the path to the cache with the R option
`R.cache.rootPath` or the environment variable `R_CACHE_ROOTPATH` to an
existent path before you call the styler API.

## Invalidation

The cache is specific to a version of styler by default, because
different versions potentially format code differently. This means after
upgrading styler or a style guide you use, the cache will be re-built.

## Mechanism and size

The cache works by storing hashed output code as a whole and by
expression, which is why it takes zero space on disk (the cache is a
directory with empty files which have the hash of output code as name).

The cache literally takes zero space on your disk, only the inode, and
you can always manually clean up with
[`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md) or
just go to the directory where the cache lives (find it with
[`cache_info()`](https://styler.r-lib.org/reference/cache_info.md)) and
manually delete files.

## Using a cache for styler in CI/CD

If you want to set up caching in a CI/CD pipeline, we suggest to set the
`{R.cache}` root path to a directory for which you have the cache
enabled. This can often be set in config files of CI/CD tools, e.g. see
the [Travis documentation on
caching](https://docs.travis-ci.com/user/caching).

## See also

Other cache managers:
[`cache_activate()`](https://styler.r-lib.org/reference/cache_activate.md),
[`cache_clear()`](https://styler.r-lib.org/reference/cache_clear.md),
[`cache_info()`](https://styler.r-lib.org/reference/cache_info.md)
