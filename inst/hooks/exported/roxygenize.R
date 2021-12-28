#!/usr/bin/env Rscript

"Run roxygen2::roxygenize()

Obviously, this hook is only activated when any staged file passes the filter
specified in .pre-commit-hooks.yaml. Then, we check the time stamp of the last
time we ran the hook. If any of our R files is younger than that, we consider
running the hook. We next use {git2r} to inspect the cached diff of all .R files
in the R/ directory, and not the files passed to this hook. If we find any
roxygen2 comment in the diff, we run `roxygen2::roxygenize(). The preliminary
use case for this is when we previously attempted to commit but check failed, so
on the second try without any other files changed, it will succeed.
This check should run *after* check that modify the files that are passed to
them (like styler) because they will never modify their input .R files.

Usage:
  roxygenize [--no-warn-cache] <files>...

Options:
  --no-warn-cache  Suppress the warning about a missing permanent cache.

" -> doc
arguments <- docopt::docopt(doc)
if (packageVersion("precommit") < "0.1.3.9010") {
  rlang::abort(paste(
    "This hooks only works with the R package {precommit} >= 0.1.3.9010",
    'Please upgrade with `remotes::install_github("lorenzwalthert/precommit@v0.1.3.9010")`.'
  ))
} else {
  precommit::may_require_permanent_cache(arguments$no_warn_cache)
  precommit::roxygen_assert_additional_dependencies()
}

path_relative_cache <- precommit::dirs_R.cache("roxygenize")
wd <- list(getwd())
cache <- R.cache::loadCache(key = wd, dirs = path_relative_cache)
rd_files_before_roxygen <- list.files("man", pattern = "\\.Rd$")

if (!is.null(cache)) {
  candidates <- intersect(
    list.files(c("R", "man"), full.names = TRUE),
    arguments$files
  )
  all_files <- file.info(candidates)
  last_modified <- max(all_files$mtime)
  if (last_modified > cache[[1]]) {
    precommit::roxygenize_with_cache(key = wd, dirs = path_relative_cache)
  }
} else {
  precommit::roxygenize_with_cache(key = wd, dirs = path_relative_cache)
}

rd_files_after_roxygen <- list.files("man", pattern = "\\.Rd$")

if (length(setdiff(rd_files_after_roxygen, rd_files_before_roxygen)) > 0) {
  rlang::abort("Please commit the new `.Rd` files in `man/`.")
}
