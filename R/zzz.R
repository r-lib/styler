.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.colored_print.vertical = TRUE,
    styler.ignore_start = "# styler: off",
    styler.ignore_stop = "# styler: on"
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  invisible()
}
