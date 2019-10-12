.onLoad <- function(libname, pkgname) {

  op <- options()
  op.precommithooks <- list(
    precommithooks.executable = derive_path_precommit_exec()
  )
  toset <- !(names(op.precommithooks) %in% names(op))
  if (any(toset)) options(op.precommithooks[toset])
  invisible()
}
