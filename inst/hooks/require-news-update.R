#! /usr/local/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
if (!any(args == "NEWS.md")) {
  rlang::abort("Must have a news entry before pushing.")
}
