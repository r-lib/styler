.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.addins_style_transformer = "styler::tidyverse_style()",
    styler.cache_root = NULL,
    styler.cache_name = styler_version,
    styler.colored_print.vertical = TRUE,
    styler.ignore_start = "# styler: off",
    styler.ignore_stop = "# styler: on",
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
  if (ask && runif(1) > 0.9 && is.null(getOption("styler.cache_root"))) {
    cli::cli_inform(paste0(
      "The R option `styler.cache_root` is not set, which means the cache ",
      "will get cleaned up after 6 days (and repeated styling will be slower).",
      " To keep cache files longer, set ",
      "the option to location within the {{R.cache}} cache where you want to ",
      "store the cache, e.g. `\"styler-perm\"`.\n\n",
      "options(styler.cache_root = \"styler-perm\")\n\n",
      "in your .Rprofile. Note that the cache literally ",
      "takes zero space on your disk, only the inode, and you can always ",
      "manually clean up with `styler::cache_clear()`, and if you update the ",
      "{{styler}} package, the cache is removed in any case. To ignore this ",
      "message in the future, set the default explictly to \"styler\" with\n\n",
      "options(styler.cache_root = \"styler\")\n\nin your `.Rprofile`. This ",
      "message will only be displayed once in a while.\n"
    ))
  }
  options(styler.cache_root = "styler")
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
