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
  all_cached <- list.files(
    R.cache::getCachePath(c("styler", styler_version)),
    full.names = TRUE, recursive = TRUE
  )
  date_boundary <- Sys.time() - 60L * 60L * 24L * 6L
  file.remove(
    all_cached[file.info(all_cached)$mtime < date_boundary]
  )
}


remove_cache_old_versions <- function() {
  dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
  old_package_dirs <- dirs[basename(dirs) != as.character(styler_version)]
  purrr::walk(old_package_dirs, unlink, recursive = TRUE, force = TRUE)
}

# nocov end
