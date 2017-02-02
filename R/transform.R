transform_files <- function(files, transformers) {
  changed <- BBmisc::vlapply(files, apply_transformers_on_file, transformers)
  changed <- files[changed]

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

apply_transformers_on_file <- function(file, transformers) {
  text <- readLines(file)
  new_text <- Reduce(
    function(text, transformer) transformer(text),
    transformers,
    init = text)
  if (any(text != new_text)) {
    writeLines(new_text, file)
    TRUE
  } else {
    FALSE
  }
}
