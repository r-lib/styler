#' Make sure text after styling results in the same hash as text before styling
#' if it is indeed identical.
#' @param x A character vector.
#' @keywords internal
hash_standardize <- function(x) {
  x <- ensure_last_is_empty(x)
  Encoding(x) <- "UTF-8"
  list(x)
}

#' Where is the cache?
#'
#' Finds the path to the cache and creates it if it does not exist.
#' @param cache_subdir The subdir of the cache. Is equivalent to the
#'   R.cache subdir when adding "styler" as a parent directory to
#'   `cache_subdir`.
#' @keywords internal
cache_find_path <- function(cache_subdir = NULL) {
  if (is.null(cache_subdir)) {
    cache_subdir <- get_cache_subdir()
  }
  R.cache::getCachePath(c("styler", cache_subdir))
}

get_cache_subdir <- function() {
  getOption("styler.cache_subdir")
}
