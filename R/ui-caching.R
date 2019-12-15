#' Clear the cache
#'
#' Clears the cache that stores which files are already styled. You won't be
#' able to undo this. Note that the file corresponding to the cache (a folder
#' on your file stystem) won't be deleted, but it will be empty after calling
#' `cache_clear`.
#' @param cache_name The name of the styler cache to use. If
#'   `NULL`, the option "styler.cache_name" is considered which defaults to
#'   the version of styler used.
#' @details
#' Each version of styler has it's own cache by default, because styling is
#' potentially different with different versions of styler.
#' @param ask Whether or not to interactively ask the user again.
#' @family cache managers
#' @export
cache_clear <- function(cache_name = NULL, ask = TRUE) {
  assert_R.cache_installation(installation_only = TRUE)
  path_cache <- cache_find_path(cache_name)
  R.cache::clearCache(path_cache, prompt = ask)
  cache_deactivate(verbose = FALSE)
}


#' Remember the past to be quicker in the future
#'
#' styler by default uses caching. It may prompt you to install the R package
#' `R.cache` the first time you want to use it. R.cache will also ask you to let
#' it create a permanent cache on your file system that styler will use.
#' This is needed if you want to cache across R sessions and not just within.
#' The cache is specific to a version of styler by default, because different
#' versions potentially format code differently. This means after upgrading
#' styler or a style guide you use, the cache will be re-built.
#' See [cache_info()],
#' [cache_activate()], [cache_clear()] for utilities to manage the cache.
#' @name caching
NULL

#' Show information about the styler cache
#'
#' Gives information about the cache. Note that the size consumed by the cache
#' will always be displayed as zero because all the cache does is creating an
#' empty file of size 0 bytes for every cached expression. The innode is
#' excluded from this displayed size but negligible.
#' @param cache_name The name of the cache for which to show details. If
#'   `NULL`, the active cache is used. If none is active the cache corresponding
#'   to the installed styler version is used.
#' @param format Either "lucid" for a summary emitted with [base::cat()],
#'   "tabular" for a tabular summary from [base::file.info()] or "both" for
#'   both.
#' @family cache managers
#' @export
cache_info <- function(cache_name = NULL, format = "both") {
  assert_R.cache_installation(installation_only = TRUE)
  rlang::arg_match(format, c("tabular", "lucid", "both"))
  path_cache <- cache_find_path(cache_name)
  files <- list.files(path_cache, full.names = TRUE)
  file_info <- file.info(files) %>%
    as_tibble()
  tbl <- tibble(
    n = nrow(file_info),
    size = sum(file_info$size),
    last_modified = suppressWarnings(max(file_info$mtime)),
    created = file.info(path_cache)$ctime,
    location = path_cache,
    activated = cache_is_activated(cache_name)
  )
  if (format %in% c("lucid", "both")) {
    cat(
      "Size:\t\t", tbl$size, " bytes (", tbl$n, " cached expressions)",
      "\nLast modified:\t", as.character(tbl$last_modified),
      "\nCreated:\t", as.character(tbl$created),
      "\nLocation:\t", path_cache,
      "\nActivated:\t", tbl$activated,
      "\n",
      sep = ""
    )
  }
  if (format == "tabular") {
    tbl
  } else if (format == "both") {
    invisible(tbl)
  }
}

#' Activate or deactivate the styler cache
#'
#' Helper functions to control the behavior of caching. Simple wrappers around
#' [base::options()].
#' @inheritParams cache_clear
#' @param verbose Whether or not to print an informative message about what the
#'   function is doing.
#' @family cache managers
#' @export
cache_activate <- function(cache_name = NULL, verbose = TRUE) {
  assert_R.cache_installation(installation_only = TRUE)
  if (!is.null(cache_name)) {
    options("styler.cache_name" = cache_name)
  } else {
    options("styler.cache_name" = styler_version)
  }
  path <- cache_find_path(cache_name)
  if (verbose) {
    cat(
      "Using cache ", cache_get_name(), " at ",
      path, ".\n",
      sep = ""
    )
  }
  invisible(path)
}

#' @rdname cache_activate
#' @export
cache_deactivate <- function(verbose = TRUE) {
  options("styler.cache_name" = NULL)

  if (verbose) {
    cat("Deactivated cache.\n")
  }
}
