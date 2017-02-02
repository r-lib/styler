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

gsub_in_files <- function(files, search, replace) {
  changed <- BBmisc::vlapply(files, gsub_in_file, search, replace)
  files[changed]
}

gsub_in_file <- function(file, search, replace) {
  text <- readLines(file)
  roxy_lines <- grep("^\\s*#'", text, perl = TRUE)
  if (length(roxy_lines) == 0) return(FALSE)
  new_text <- text
  new_text[roxy_lines] <- gsub(search, replace, text[roxy_lines], perl = TRUE)
  writeLines(new_text, file)
  any(text != new_text)
}
