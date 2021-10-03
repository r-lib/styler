write_union <- function(path, lines) {
  stopifnot(is.character(lines))
  if (file.exists(path)) {
    existing_lines <- read_utf8(path)
  } else {
    existing_lines <- character()
  }
  new <- setdiff(lines, existing_lines)
  if (length(new) == 0) {
    return(invisible(FALSE))
  }
  cli::cli_alert_success("Adding {new} to {path}.")
  all <- c(existing_lines, new)
  write_utf8(path, all)
}


write_utf8 <- function(path, text) {
  opts <- options(encoding = "native.enc")
  withr::defer(options(opts))
  writeLines(enc2utf8(text), path, useBytes = TRUE)
}

read_utf8 <- function(path) {
  opts <- options(encoding = "native.enc")
  withr::defer(options(opts))
  readLines(path, encoding = "UTF-8", warn = FALSE)
}
