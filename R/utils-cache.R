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
#' This is used to determine if caching already corresponds to a style guide.
#' @details
#'
#' We need to compare:
#'
#' * text to style. Will be passed to hash function as is.
#' * styler version. Not an issue because for every version of styler, we build
#'   a new cache.
#' * transformers. Cannot easily hash them because two environments won't be
#'   identical even if they contain the same objects (see 'Experiments'). Simple
#'   `as.character(transformers)` will not consider infinitively recursive
#'   code dependencies.
#'   To fix this, transformers must have names and version number as described
#'   in [create_style_guide()]. Now, the only way to fool the cache invalidation
#'   is to replace a transformer with the same function body (but changing
#'   the function definition of the functions called in that body) interactively
#'   without changing version number of name at the same time.
#' @section Experiments:
#'
#' There is unexplainable behavior in conjunction with hashing and
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
  list(
    text = hash_standardize(text),
    style_guide_name = transformers$style_guide_name,
    style_guide_version = transformers$style_guide_version,
    style_guide_text = as.character(transformers)
  )
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

