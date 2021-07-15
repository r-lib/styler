#!/usr/bin/env Rscript

files <- commandArgs(trailing = TRUE)
no_debug_statement <- function(path) {
  pd <- getParseData(parse(path, keep.source = TRUE))
  if (any(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"] %in% c("debug", "debugonce"))) {
    stop("File `", path, "` contains a `debug()` or `debugonce()` statement.", call. = FALSE)
  }
}

for (file in files) {
  temp <- no_debug_statement(file)
}
