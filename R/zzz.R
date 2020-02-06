.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.colored_print.vertical = TRUE,
    styler.cache_name = NULL,
    styler.addins_style_transformer = "styler::tidyverse_style()",
    styler.ignore_start = "# styler: off",
    styler.ignore_stop = "# styler: on"
  )
  rlang::warn("Caching feature temporarily disabled and not encouraged to use. See https://github.com/r-lib/styler/issues/584")
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  invisible()
}
