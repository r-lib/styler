#!/usr/bin/env Rscript
# This hook checks that the `exclude:` key in the pre-commit config files
# are alphabetically ordered. This is helpful for manual searching plus also
# groups the patterns, i.e. extensions, files everywhere, directories
library(magrittr)
args <- commandArgs(trailingOnly = TRUE)
run_one_file <- function(file) {
  config <- yaml::read_yaml(file) # pre-commit filter
  print(config)
  ours <- which(purrr::map_chr(config$repos, "repo") == "https://github.com/lorenzwalthert/precommit")
  nme <- purrr::map_chr(config$repos[[ours]]$hooks, "id")

  regex <- config$repos[[1]]$hooks[[which(nme == "spell-check")]]$exclude %>%
    strsplit(" +") %>%
    unlist()

  without_mask <- regex[c(-1, -length(regex))]
  if (any(without_mask != sort(without_mask))) {
    cat(paste0(
      "regular expressions not sorted for spell check hook, paste this ",
      "into the pre-commit config file: under the `exclude:` key:"
    ))

    cat(c("", regex[1], sort(without_mask), regex[length(regex)]), sep = "\n          ")
    rlang::abort("Execution halted.")
  }
}

purrr::walk(args, run_one_file)
