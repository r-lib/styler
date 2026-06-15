# Delete a cache or temp directory

For safety, `path` is only deleted if it is a sub-directory of a
temporary directory or user cache. Since this function relies on
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), it early
returns `FALSE` on `R < 4.0.0`.

## Usage

``` r
delete_if_cache_directory(path)
```

## Arguments

- path:

  Absolute path to a directory to delete.

## Value

`TRUE` if anything was deleted, `FALSE` otherwise.
