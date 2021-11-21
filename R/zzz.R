.onLoad <- function(libname, pkgname) {
  op <- options()
  op.precommit <- list(
    precommit.executable = path_derive_precommit_exec(),
    precommit.block_install_hooks = FALSE,
    precommit.ci = "native"
  )
  toset <- !(names(op.precommit) %in% names(op))
  if (any(toset)) options(op.precommit[toset])
  invisible()
}
