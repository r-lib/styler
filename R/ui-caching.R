#' Clear the cache
#'
#' Clears the cache that stores which files are already styled. You won't be
#' able to undo this. Note that the file corresponding to the cache (a folder
#' on your file system) won't be deleted, but it will be empty after calling
#' `cache_clear`.
#' @param cache_name The name of the styler cache to use. If
#'   `NULL`, the option "styler.cache_name" is considered which defaults to
#'   the version of styler used.
#' @details
#' Each version of styler has its own cache by default, because styling is
#' potentially different with different versions of styler.
#' @param ask Whether or not to interactively ask the user again.
#' @family cache managers
#' @export
cache_clear <- function(cache_name = NULL, ask = TRUE) {
  path_cache <- cache_find_path(cache_name)
  R.cache::clearCache(path_cache, prompt = ask)
  cache_deactivate(verbose = FALSE)
}


#' Remember the past to be quicker in the future
#'
#' Caching makes styler faster on repeated styling and is shared across all APIs
#' (e.g. `style_text()` and Addin). That means if you style code that already
#' complies to a style guide and you have previously styled that code, it will
#' be quicker.
#'
#' @section Configuring the cache:
#'
#' To comply with the CRAN policy, \{styler\} will by default clean up cache files
#' that are older than 6 days. This implies that you loose the benefit of the cache
#' for the files not styled in the last 6 days.
#'
#' If you want to avoid this, i.e., if you want the cache to last longer, you can use the
#' R option `styler.cache_root` to opt for an indefinitely long-lived cache by setting it to
#' `options(styler.cache_root = "styler-perm")`.
#'
#' If you are happy with the cache being cleared after 6 days, you can confirm the default and
#' silence this message by setting it instead to `options(styler.cache_root = "styler")`.
#'
#' You can make this change in your `.Rprofile` using `usethis::edit_r_profile()`.
#'
#' @section Manage the cache:
#' See [cache_info()],[cache_activate()] or [cache_clear()] for utilities to
#' manage the cache. You can deactivate it altogether with [cache_deactivate()].
#' Since we leverage `{R.cache}` to manage the cache, you can also use any
#' `{R.cache}` functionality to manipulate it.
#'
#' In some cases, you want to use a non-standard cache location. In
#' that situation, you can set the path to the cache with the R option
#' `R.cache.rootPath` or the environment variable `R_CACHE_ROOTPATH` to an
#' existent path before you call the styler API.
#'
#' @section Invalidation:
#' The cache is specific to a version of styler by default, because different
#' versions potentially format code differently. This means after upgrading
#' styler or a style guide you use, the cache will be re-built.
#'
#' @section Mechanism and size:
#' The cache works by storing hashed output code as a whole and by expression,
#' which is why it takes zero space on disk (the cache is a directory with
#' empty files which have the hash of output code as name).
#'
#' The cache literally takes zero space on your disk, only the inode, and you
#' can always manually clean up with [cache_clear()] or just go to the
#' directory where the cache lives (find it with [cache_info()]) and manually
#' delete files.
#'
#' @section Using a cache for styler in CI/CD:
#' If you want to set up caching in a CI/CD pipeline, we suggest to set the
#' `{R.cache}` root path to a directory for which you have the cache enabled.
#' This can often be set in config files of CI/CD tools, e.g. see the
#' [Travis documentation on caching](https://docs.travis-ci.com/user/caching).
#'
#' @name caching
#' @family cache managers
NULL

#' Show information about the styler cache
#'
#' Gives information about the cache. Note that the size consumed by the cache
#' will always be displayed as zero because all the cache does is creating an
#' empty file of size 0 bytes for every cached expression. The inode is
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
  rlang::arg_match(format, c("tabular", "lucid", "both"))
  path_cache <- cache_find_path(cache_name)
  files <- list.files(path_cache, full.names = TRUE)
  file_info <- file.info(files)

  tbl <- styler_df(
    n = nrow(file_info),
    size = sum(file_info$size),
    last_modified = suppressWarnings(max(file_info$mtime)),
    created = file.info(path_cache)$ctime,
    location = path_cache,
    activated = cache_is_activated(cache_name),
    stringsAsFactors = FALSE
  )

  if (any(c("lucid", "both") == format)) {
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
#'
#' @family cache managers
#' @export
cache_activate <- function(cache_name = NULL,
                           verbose = !getOption("styler.quiet", FALSE)) {
  options("styler.cache_name" = cache_name %||% styler_version)
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
cache_deactivate <- function(verbose = !getOption("styler.quiet", FALSE)) {
  options("styler.cache_name" = NULL)

  if (verbose) {
    cat("Deactivated cache.\n")
  }
}
