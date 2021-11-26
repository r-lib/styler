#' Check if the R cache is persistent
#' @keywords internal
has_persistent_R.cache <- function() {
  temp_dirs <- path_if_exist(
    fs::path_norm(fs::path_dir(tempdir())),
    "/tmp", "/var/tmp"
  )
  !fs::path_has_parent(
    fs::path_norm(R.cache::getCacheRootPath()), # when no cache exists, it create temp cache
    temp_dirs
  ) %>%
    any()
}

#' Issue a warning if `{R.cache}` uses temporary cache only
#'
#' This function is only exported for use in hook scripts, but it's not intended
#' to be called by the end-user directly.
#' @param temp_cache_is_enough Whether a temporary cache is accepted or not. `TRUE`
#'   means no warning will be issued, `FALSE` means a warning will be issued if
#'   no permanent cache is available.
#' @family hook script helpers
#' @export
may_require_permanent_cache <- function(temp_cache_is_enough = FALSE) {
  if (has_persistent_R.cache()) {
    cat("Using persistent cache at", R.cache::getCacheRootPath(), "\n")
  } else {
    if (temp_cache_is_enough) {
      cat("Using temporary cache at", R.cache::getCacheRootPath(), "\n")
    } else {
      cat(paste0(
        "You don't have a permanent cache directory set up with {R.cache}. ",
        "This means you won't get significant speedups for some hooks. ",
        "Create a permanent cache in an interactive R session by \n\n1) calling ",
        "`R.cache::getCachePath()` and confirm the prompt or \n\n2) ",
        "non-interactively by setting the environment variable ",
        "`R_CACHE_ROOTPATH` to the location you want to put the cache. \n\nYou can ",
        "silent this warning with setting `args: [--no-warn-cache]` in your ",
        ".pre-commit-config.yaml.\n\n"
      ))
    }
  }
}
