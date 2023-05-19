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

delete_temp_directory_if_empty <- function(path) {
  if (grepl(tools::R_user_dir("R.cache", which = "cache"), path, fixed = TRUE)) {
    all_files <- list.files(path,
      full.names = TRUE,
      recursive = TRUE,
      all.files = FALSE
    )
    if (length(all_files) < 1L) {
      unlink(path, recursive = TRUE)
      return(TRUE)
    }
    return(FALSE)
  } else {
    rlang::abort(
      "Can only delete absolute paths under `tools::R_user_dir('R.cache')"
    )
  }
}


ask_to_switch_to_non_default_cache_root <- function(ask = interactive()) {
  if (ask && stats::runif(1L) > 0.9 && is.null(getOption("styler.cache_root"))) {
    ask_to_switch_to_non_default_cache_root_impl()
    options(styler.cache_root = "styler")
  }
}


ask_to_switch_to_non_default_cache_root_impl <- function() {
  rlang::inform(paste0(
    "{styler} cache is cleared after 6 days. ",
    "See `?styler::caching` to configure differently or silence this message."
  ))
}

remove_old_cache_files <- function() {
  path_version_specific <- R.cache::getCachePath(c("styler", styler_version))
  all_cached <- list.files(
    path_version_specific,
    full.names = TRUE, recursive = TRUE
  )
  date_boundary <- Sys.time() - 60L * 60L * 24L * 6L
  file.remove(
    all_cached[file.info(all_cached)$mtime < date_boundary]
  )
  path_styler_specific <- dirname(path_version_specific)
  path_r_cache_specific <- dirname(path_styler_specific)
  purrr::walk(
    c(path_version_specific, path_styler_specific, path_r_cache_specific),
    delete_temp_directory_if_empty
  )
}


remove_cache_old_versions <- function() {
  dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
  old_package_dirs <- dirs[basename(dirs) != as.character(styler_version)]
  purrr::walk(old_package_dirs, unlink, recursive = TRUE, force = TRUE)
}

# nocov end
