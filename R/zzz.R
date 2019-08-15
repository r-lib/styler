.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "trimws")
  op <- options()
  op.styler <- list(
    styler.colored_print.vertical = TRUE,
    styler.use_cache = TRUE
  )
  toset <- !(names(op.styler) %in% names(op))
  if (any(toset)) options(op.styler[toset])
  invisible()
}
