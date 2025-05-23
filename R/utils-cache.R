#' Standardize text for hashing
#'
#' Make sure text after styling results in the same hash as text before styling
#' if it is indeed identical. This function expects trailing blank lines in
#' `text` were removed prior to passing it to this function.
#' @param text A character vector.
#' @keywords internal
hash_standardize <- function(text) {
  text %>%
    convert_newlines_to_linebreaks() %>%
    enc2utf8() %>%
    paste(collapse = "\n") %>%
    list()
}

#' Check if text is cached
#'
#' This boils down to check if the hash exists at the caching dir as a file.
#' @param text Passed to [cache_make_key()] to generate a key.
#' @param transformers Passed to [cache_make_key()] to generate a key.
#' @param more_specs Passed to [cache_make_key()] to generate a key.
#' @param cache_dir The caching directory relative to the `.Rcache` root to
#'   look for a cached value.
#' @keywords internal
is_cached <- function(text,
                      transformers,
                      more_specs,
                      cache_dir = get_cache_dir()) {
  R.cache::generateCache(
    key = cache_make_key(text, transformers, more_specs),
    dirs = cache_dir
  ) %>%
    file.exists()
}


#' Make a key for `R.cache`
#'
#' This is used to determine if caching already corresponds to a style guide.
#' @param text Code to create a cache for. This should be styled text, as the
#'   approach used by styler does not cache input, but styled code.
#' @param transformers A list of transformer functions, because we can only
#'   know if text is already correct if we know which transformer function it
#'   should be styled with.
#' @param more_specs A named vector coercible to character that determines the
#'   styling but are style guide independent, such as `include_roxygen_examples`
#'   or `base_indention`.
#'
#' @details
#' We need to compare:
#'
#' * text to style. Will be passed to hash function as is.
#' * styler version. Not an issue because for every version of styler, we build
#'   a new cache.
#' * transformers. Cannot easily hash them because two environments won't be
#'   identical even if they contain the same objects (see 'Experiments'). Simple
#'   `as.character(transformers)` will not consider infinitely recursive
#'   code dependencies.
#'   To fix this, transformers must have names and version number as described
#'   in [create_style_guide()]. Now, the only way to fool the cache invalidation
#'   is to replace a transformer with the same function body (but changing
#'   the function definition of the functions called in that body) interactively
#'   without changing version number of name at the same time.
#'   Remaining problem: `purrr::partial()` calls will render generic code, e.g.
#'   see `as.character(list(purrr::partial(sum, x = 4)))`. For that reason,
#'   all arguments passed to a `purrr::partial()` call must be put in the
#'   style guide under `more_specs_style_guide`.
#'
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
#'   equal (see 'Examples').
#' * However, under some circumstances, it does: Commit 9c94c022 (if not
#'   overwritten / rebased by now) contains a reprex. Otherwise, search for
#'   43219ixmypi in commit messages and restore this commit to reproduce the
#'   behavior.
#'
#' @examples
#' add <- function(x, y) {
#'   x + y
#' }
#' add1 <- purrr::partial(add, x = 1)
#' add2 <- purrr::partial(add, x = 1)
#' identical(add1, add2)
#' identical(digest::digest(add1), digest::digest(add2))
#' identical(digest::digest(styler::tidyverse_style()), digest::digest(styler::tidyverse_style()))
#' @keywords internal
cache_make_key <- function(text, transformers, more_specs) {
  list(
    text = hash_standardize(text),
    style_guide_name = transformers$style_guide_name,
    style_guide_version = transformers$style_guide_version,
    more_specs_style_guide = set_names(
      as.character(transformers$more_specs_style_guide),
      names(transformers$more_specs_style_guide)
    ),
    more_specs = more_specs
  )
}

#' Where is the cache?
#'
#' Finds the path to the cache and creates it if it does not exist.
#' @inheritParams cache_clear
#' @keywords internal
cache_find_path <- function(cache_name = NULL) {
  cache_name <- cache_get_or_derive_name(cache_name)
  R.cache::getCachePath(get_cache_dir(cache_name))
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
    return(!is.null(current_cache))
  }

  if (!is.null(current_cache)) {
    return(cache_name == current_cache)
  }

  FALSE
}

#' Cache text
#'
#' Splits `text` into expressions and adds these to the cache. Note that
#' top-level comments are **not** cached because caching and in particular
#' checking  if they are cached is too expensive. Comments may be cached as part
#' of the whole text (as opposed to on an expression by expression basis) using
#' `cache_write()` directly. Also, we must not cache stylerignore sequence,
#' because we might see the same expression that does not comply with the style
#' guide outside a stylerignore sequence and wrongly think we should leave it as
#' is.
#' @param text A character vector with one or more expressions.
#' @inheritParams cache_write
#' @keywords internal
cache_by_expression <- function(text,
                                transformers,
                                more_specs) {
  expressions <- parse(text = text, keep.source = TRUE) %>%
    utils::getParseData(includeText = TRUE)
  if (env_current$any_stylerignore) {
    expressions <- add_stylerignore(expressions)
  } else {
    expressions$stylerignore <- rep(FALSE, length(expressions$text))
  }
  # TODO base_indention should be set to 0 on write and on read for expressions
  # (only) to make it possible to use the cache for expressions with different
  # indention. when not the whole input text is cached, we go trough all
  # expressions and check if they are cached, if yes, we take the input (from
  # which the indention
  # was removed via parse, same as it is in cache_by_expression) and add the
  # base indention.
  map(
    expressions[expressions$parent == 0L & expressions$token != "COMMENT" & !expressions$stylerignore, "text"],
    cache_write,
    transformers = transformers, more_specs
  )
}


#' Write to the cache
#'
#' @inheritParams cache_make_key
#' @keywords internal
cache_write <- function(text, transformers, more_specs) {
  R.cache::generateCache(
    key = cache_make_key(text, transformers, more_specs),
    dirs = get_cache_dir()
  ) %>%
    file.create()
}

styler_version <- unlist(unname(read.dcf("DESCRIPTION")[, "Version"]), use.names = FALSE)

cache_get_name <- function() {
  getOption("styler.cache_name")
}

cache_get_or_derive_name <- function(cache_name = NULL) {
  cache_name <- cache_name %||% cache_get_name()
  cache_name <- cache_name %||% styler_version
  cache_name
}

get_cache_dir <- function(cache_name = cache_get_name()) {
  c(getOption("styler.cache_root", "styler"), cache_name)
}


#' Create more specs
#'
#' Syntactic sugar for creating more specs. This is useful when we want to add
#' more arguments (because we can search for this function in the source code).
#' @keywords internal
cache_more_specs <- function(include_roxygen_examples,
                             base_indention) {
  list(
    include_roxygen_examples = include_roxygen_examples,
    base_indention = base_indention,
    ignore_alignment = getOption("styler.ignore_alignment", FALSE),
    ignore_start = getOption("styler.ignore_start", .default_ignore_start),
    ignore_stop = getOption("styler.ignore_start", .default_ignore_stop)
  )
}
