.onLoad <- function(libname, pkgname) {
  op <- options()
  op.precommit <- list(
    precommit.executable = path_derive_precommit_exec(),
    precommit.block_install_hooks = FALSE,
    precommit.ci = "native"
  )
  toset <- !(names(op.precommit) %in% names(op))
  if (any(toset)) options(op.precommit[toset])
  if (interactive()) ensure_renv_precommit_compat()
  invisible()
}

# here is only used as a default argument, hence the R CMD check warning
# Namespace in Imports field not imported from
if (FALSE) {
  here::here()
}
