.onLoad <- function(libname, pkgname) {
  op <- options()
  op.precommit <- list(
    precommit.executable = path_derive_precommit_exec()
  )
  toset <- !(names(op.precommit) %in% names(op))
  if (any(toset)) options(op.precommit[toset])
  invisible()
}
