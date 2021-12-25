#!/usr/bin/env Rscript
files <- commandArgs(trailing = TRUE)

out <- lapply(files, function(path) {
  is_rmd <- grepl("\\.[rR]md$", path)
  if (is_rmd) {
    path_ <- precommit::robust_purl(path)
  } else {
    path_ <- path
  }

  tryCatch(
    parse(path_),
    error = function(x) stop("File ", path, " is not parsable", call. = FALSE)
  )
})
