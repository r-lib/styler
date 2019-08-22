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
#' @inheritParams cache_clear
#' @keywords internal
cache_find_path <- function(cache_name = NULL) {
  cache_name <- cache_get_or_derive_name(cache_name)
  R.cache::getCachePath(c("styler", cache_name))
}

#' Check if a cache is activated
#'
#' @param cache_name The name of the cache to check. If `NULL`, we check if
#'   any cache is activated. If not `NULL`, we check if a specific cache is
#'   activated.
#' @keywords internal
cache_is_activated <- function(cache_name = NULL) {
  current_cache <- cache_get_name()
  if (is.null(cache_name)) {
    !is.null(current_cache)
  } else if (!is.null(current_cache)) {
    cache_name == current_cache
  } else {
    FALSE
  }
}

cache_derive_name <- function() {
  utils::packageDescription("styler", fields = "Version")
}

cache_get_name <- function() {
  getOption("styler.cache_name")
}

cache_get_or_derive_name <- function(cache_name) {
  if (is.null(cache_name)) {
    cache_name <- cache_get_name()
    if (is.null(cache_name)) {
      cache_name <- cache_derive_name()
    }
  }
  cache_name
}

