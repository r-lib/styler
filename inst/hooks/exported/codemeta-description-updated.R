#!/usr/bin/env Rscript

# adapted from https://github.com/lorenzwalthert/precommit/blob/f4413cfe6282c84f7176160d06e1560860c8bd3d/inst/hooks/exported/readme-rmd-rendered
if (!file.exists("DESCRIPTION")) {
  rlang::abort("No `DESCRIPTION` found in repository.")
}

if (!file.exists("codemeta.json")) {
  rlang::abort("No `codemeta.json` found in repository.")
}


codemeta_outdated <- file.info("DESCRIPTION")$mtime > file.info("codemeta.json")$mtime
if (codemeta_outdated) {
  rlang::abort("codemeta.json is out of date; please re-run codemetar::write_codemeta().")
}
