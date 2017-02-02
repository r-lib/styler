transform_files <- function(files, transformers) {
  changed <- lapply(
    transformers, function(fun) fun(files))
  changed <- sort(unique(unlist(changed)))

  if (length(changed) > 0) {
    message(
      "Changed ", length(changed), " files: ",
      paste(changed, collapse = ", "),
      ". Please review the changes carefully!")
  } else {
    message("No files changed")
  }
  invisible(changed)
}
