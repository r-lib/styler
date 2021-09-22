.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.addins_style_transformer = "styler::tidyverse_style()",
    styler.cache_name = styler_version,
    styler.colored_print.vertical = TRUE,
    styler.ignore_start = "# styler: off",
    styler.ignore_stop = "# styler: on",
    styler.quiet = FALSE,
    styler.test_dir_writable = TRUE,
    styler.interactive_ask_remove_old_caches = TRUE
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  remove_cache_old_versions()
  remove_old_cache_files()
  invisible()
}

remove_old_cache_files <- function() {
  all_cached <- list.files(
    R.cache::getCachePath(c("styler", styler_version)),
    full.names = TRUE
  )
  date_boundary <- Sys.time() - 60 * 60 * 24 * 6
  file.remove(
    all_cached[file.info(all_cached)$mtime < date_boundary]
  )
}

#' Ask people to remove the cache
#'
#' The way RStudio Startup works does not allow o read the prompt for some
#' reasons (https://stackoverflow.com/questions/55772436/readline-does-not-prompt-user-input-from-rprofile-site-in-rstudio)
#' So we better don't use the prompt and issue a message only.
#' @keywords internal
remove_cache_old_versions <- function() {
  dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
  if (length(dirs) < 1) {
    return()
  }
  old_package_dirs <- dirs[basename(dirs) != as.character(styler_version)]
  if (length(old_package_dirs) < 1) {
    return()
  }
  purrr::walk(old_package_dirs, function(dir) {
    cache_clear(basename(dir), ask = FALSE)
    file.remove(dir)
  })
}
