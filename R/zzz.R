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
  remind_removing_old_cache()
  invisible()
}

#' Ask people to remove the cache
#'
#' The way RStudio Startup works does not allow o read the prompt for some
#' reasons (https://stackoverflow.com/questions/55772436/readline-does-not-prompt-user-input-from-rprofile-site-in-rstudio)
#' So we better don't use the prompt and issue a message only.
#' @keywords internal
remind_removing_old_cache <- function() {
  if (interactive() && getOption("styler.interactive_ask_remove_old_caches", TRUE)) {
    dirs <- list.dirs(R.cache::getCachePath("styler"), recursive = FALSE)
    if (length(dirs) < 1) {
      return()
    }
    dirs <- dirs[unname(sapply(dirs, function(x) length(list.files(x)) > 0))]
    package_versions <- package_version(basename(dirs), strict = FALSE)
    package_versions <- package_versions[!is.na(package_versions)]
    old_package_versions <- package_versions[package_versions < styler_version]
    if (length(old_package_versions) < 1) {
      return()
    }

    cmd <- glue::glue("styler::cache_clear(\"{basename(dirs)}\", ask = FALSE)") %>%
      paste0(collapse = "\n")
    cli::cli_alert_info(paste0(
      "You are using {{styler}} version {styler_version} but we found ",
      "caches for older versions of {{styler}}.\n",
      "You can delete them with the following commands:"
    ))
    cat("\n")
    cli::cli_code(cmd)
    cat("\n")
    cli::cli_alert_info(
      paste(
        "We'll remind you every time you update {{styler}}.",
        "To suppress this prompt in the future:"
      )
    )
    cat("\n")
    cli::cli_code(
      'options("styler.interactive_ask_remove_old_caches" = FALSE)'
    )
  }
}
