#' Standardize text for hashing
#'
#' Make sure text after styling results in the same hash as text before styling
#' if it is indeed identical.
#' @param text A character vector.
#' @keywords internal
hash_standardize <- function(text) {
  text %>%
    convert_newlines_to_linebreaks() %>%
    ensure_last_is_empty() %>%
    enc2utf8() %>%
    paste0(collapse = "\n") %>%
    list()
}

#' Make a key for `R.cache`
#'
#' @details
#'
#' This function standardizes text and converts transformers to character (to
#' avoid issues described in details).
#' This means that the same code in `transformers`,
#' calling other code not in `transformers` that was modified, will lead
#' styler into thinking we can use the cache, although we should not. We believe
#' this is a highly unlikely event, in particular because we already invalidate
#' the cache when the styler version changes. Hence, our cache will cause
#' styler to return *not correctly styled* code iff one of these conditions
#' holds:
#' - An improperly versioned version of styler is used, e.g. the development
#'   version on GitHub.
#' - A style guide from outside styler is used.
#'
#' Plus for both cases: the code in transformers does not change and changes in
#' code the transformers depend on result in different styling.
#' @section Experiments:
#'
#' There is unexamplainable behavior in conjunction with hashin and
#' environments:
#' * Functions created with `purrr::partial()` are not identical when compared
#'   with `identical()`
#'   ([StackOverflow](https://stackoverflow.com/questions/58656033/when-are-purrrpartial-ized-functions-identical))
#' * except when they have the exact same parent environment, which must be an
#'   object created and then passed to `purrr::partial(.env = ...)`, not
#'   created in-place.
#' * `purrr::partial()` seems to ignore `.env` after version 0.2.5, so until
#'   this is fixed, we'd have to work with version 0.2.5.
#' * Our caching backend package, `R.cache`, uses
#'   `R.cache:::getChecksum.default` (which uses `digest::digest()`) to hash the
#'   input. The latter does not seem to care if the environments are exactly
#'   equal (see 'Exampels').
#' * However, under stome circumstances, it does: Commit 9c94c022 (if not
#'   overwritten / rebased by now) contains a reprex. Otherwise, search for
#'   43219ixmypi in commit messages and restore this commit to reproduce the
#'   behavior.
#' @examples
#' add <- function(x, y) {
#' x + y
#' }
#' add1 <- purrr::partial(add, x = 1)
#' add2 <- purrr::partial(add, x = 1)
#' identical(add1, add2)
#' identical(digest::digest(add1), digest::digest(add2))
#' identical(digest::digest(styler::tidyverse_style()), digest::digest(styler::tidyverse_style()))
#' @keywords internal
cache_make_key <- function(text, transformers) {
  text <- hash_standardize(text)
  c(text = text, transformers = as.character(transformers))
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

styler_version <- unlist(unname(read.dcf("DESCRIPTION")[, "Version"]))

cache_get_name <- function() {
  getOption("styler.cache_name")
}

cache_get_or_derive_name <- function(cache_name) {
  if (is.null(cache_name)) {
    cache_name <- cache_get_name()
    if (is.null(cache_name)) {
      cache_name <- styler_version
    }
  }
  cache_name
}

