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
    styler.test_dir_writable = TRUE
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
    full.names = TRUE, recursive = TRUE
  )
  date_boundary <- Sys.time() - 60 * 60 * 24 * 6
  file.remove(
    all_cached[file.info(all_cached)$mtime < date_boundary]
  )
}


remove_cache_old_versions <- function() {
  dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
  old_package_dirs <- dirs[basename(dirs) != as.character(styler_version)]
  purrr::walk(old_package_dirs, function(dir) {
    unlink(dir, recursive = TRUE, force = TRUE)
  })
}
