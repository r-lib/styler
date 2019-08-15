.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.colored_print.vertical = TRUE,
    styler.use_cache = TRUE,
    styler.cache_subdir = packageDescription("styler", fields = "Version"),
    styler.addins_style_transformer = "styler::tidyverse_style()"
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  invisible()
}
