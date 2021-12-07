#' Issue a warning if `{R.cache}` uses temporary cache only
#'
#' This function used to check if a permanent cache was available and issue a
#' warning if not, but since {R.cache} version `0.15.0` (release date
#' 2021-04-27), a permanent directory will be used automatically, so this check
#' if redundant. the function is kept in the package for compatibility, i.e.
#' if someone updates the R package {precommit} but not the hook revisions.
#' @param temp_cache_is_enough ignored.
#' @family hook script helpers
#' @export
may_require_permanent_cache <- function(temp_cache_is_enough = FALSE) {
  if (temp_cache_is_enough) {
    rlang::warn(paste0(
      "The argument `--no-warn-cache` is deprecated and will be removed in a ",
      "future release. Please remove it from your `.pre-commit-config.yaml`."
    ))
  }
  return()
}
