#!/usr/bin/env Rscript
files <- commandArgs(trailing = TRUE)

out <- lapply(files, function(path) {
  is_rmd <- grepl("\\.[rR]md$", path)
  if (is_rmd) {
    dir <- tempdir()
    path <- knitr::purl(
      input = path,
      output = paste0(dir, "/file.R"),
      quiet = TRUE,
      documentation = FALSE
    )
  }

  tryCatch(
    parse(path),
    error = function(x) stop("File ", path, " is not parsable", call. = FALSE)
  )
})
