# nocov start
.default_ignore_start <- "styler: off"
.default_ignore_stop <- "styler: on"

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.styler <- list(
    styler.addins_style_transformer = "styler::tidyverse_style()",
    styler.cache_root = NULL,
    styler.cache_name = styler_version,
    styler.colored_print.vertical = TRUE,
    styler.ignore_alignment = FALSE,
    styler.ignore_start = .default_ignore_start,
    styler.ignore_stop = .default_ignore_stop,
    styler.quiet = FALSE,
    styler.test_dir_writable = TRUE
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  ask_to_switch_to_non_default_cache_root()
  remove_cache_old_versions()
  remove_old_cache_files()
  invisible()
}

#' Delete a cache or temp directory
#'
#' For safety, `path` is only deleted if it is a sub-directory of a temporary
#' directory or user cache. Since this function relies on `tools::R_user_dir()`,
#' it early returns `FALSE` on `R < 4.0.0`.
#' @param path Absolute path to a directory to delete.
#' @returns `TRUE` if anything was deleted, `FALSE` otherwise.
#' @keywords internal
delete_if_cache_directory <- function(path) {
  path <- normalizePath(path)
  if (getRversion() < package_version("4.0.0")) {
    return(FALSE)
  }
  designated_cache_path <- normalizePath(tools::R_user_dir("R.cache", which = "cache"), mustWork = FALSE)
  is_in_tools_cache <- startsWith(path, designated_cache_path)
  temp_dir <- normalizePath(dirname(tempdir()))
  is_in_generic_cache <- startsWith(path, temp_dir)
  if (is_in_tools_cache || is_in_generic_cache) {
    all_files <- list.files(path,
      full.names = TRUE,
      recursive = TRUE,
      all.files = FALSE
    )
    if (length(all_files) < 1L) {
      unlink(path, recursive = TRUE)
      return(TRUE)
    }
  }
  FALSE
}


ask_to_switch_to_non_default_cache_root <- function(ask = interactive()) {
  if (ask && stats::runif(1L) > 0.9 && is.null(getOption("styler.cache_root"))) {
    ask_to_switch_to_non_default_cache_root_impl()
    options(styler.cache_root = "styler")
  }
}


ask_to_switch_to_non_default_cache_root_impl <- function() {
  cli::cli_inform(paste0(
    "{{styler}} cache is cleared after 6 days. ",
    "See {.help styler::caching} to configure differently or silence this message."
  ))
}

remove_old_cache_files <- function() {
  path_version_specific <- R.cache::getCachePath(c("styler", styler_version))
  all_cached <- list.files(
    path_version_specific,
    full.names = TRUE, recursive = TRUE
  )
  date_boundary <- Sys.time() - as.difftime(6L, units = "days")
  file.remove(
    all_cached[file.info(all_cached)$mtime < date_boundary]
  )
  path_styler_specific <- dirname(path_version_specific)
  path_r_cache_specific <- dirname(path_styler_specific)
  paths <- normalizePath(
    c(path_version_specific, path_styler_specific, path_r_cache_specific)
  )
  purrr::walk(
    paths,
    delete_if_cache_directory
  )
}


remove_cache_old_versions <- function() {
  dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
  old_package_dirs <- dirs[basename(dirs) != as.character(styler_version)]
  purrr::walk(old_package_dirs, unlink, recursive = TRUE, force = TRUE)
}

# nocov end
