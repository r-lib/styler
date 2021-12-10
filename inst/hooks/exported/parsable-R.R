#!/usr/bin/env Rscript
files <- commandArgs(trailing = TRUE)

out <- lapply(files, function(path) {
  is_rmd <- grepl("\\.[rR]md$", path)
  if (is_rmd) {
    path_ <- knitr::purl(
      input = path,
      output = tempfile(fileext = ".R"),
      quiet = TRUE,
      documentation = FALSE
    )
  } else {
    path_ <- path
  }

  tryCatch(
    parse(path_),
    error = function(x) stop("File ", path, " is not parsable", call. = FALSE)
  )
})
