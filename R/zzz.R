.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.colored_print.vertical = TRUE,
    styler.cache_name = styler_version,
    styler.addins_style_transformer = "styler::tidyverse_style()",
    styler.ignore_start = "# styler: off",
    styler.ignore_stop = "# styler: on"
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  invisible()
}
