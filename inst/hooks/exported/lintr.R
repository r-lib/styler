#!/usr/bin/env Rscript


"Run lintr on R files during a precommit.
Usage:
  lintr [--warn_only] <files>...
Options:
  --warn_only  Print lint warnings instead of blocking the commit. Should be
               used with `verbose: True` in `.pre-commit-config.yaml`.
               Otherwise, lints will never be shown to the user.
" -> doc

arguments <- docopt::docopt(doc)

lintr_staged <- grepl(
  "modified:.*\\.lintr", system2("git", "status", stdout = TRUE)
)
if (any(lintr_staged)) {
  stop(
    "Unstaged changes to .lintr file. Stage the .lintr file or discard ",
    "the changes to it. ",
    call. = FALSE
  )
}

for (path in arguments$files) {
  lints <- lintr::lint(path)
  if (length(lints) > 0) {
    cat("File `", path, "` is not lint free\n", sep = "")
    rendered_lints <- capture.output(print(lints))
    cat(rendered_lints, sep = "\n")
    if (!arguments$warn_only) {
      stop("File ", path, " is not lint free", call. = FALSE)
    }
  }
}
