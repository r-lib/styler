is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_url <- function(text) {
  if (length(text) < 1) {
    return(FALSE)
  }
  conn <- file(text)
  on.exit(close(conn))
  conn %>%
    inherits("url")
}


path_if_exist <- function(...) {
  path <- c(...)
  path[fs::file_exists(path)]
}

is_conda_installation <- function() {
  grepl(
    "conda3?/envs/r-precommit/(bin|Scripts)/pre-commit(\\.exe)?",
    getOption("precommit.executable")
  )
}

is_package <- function(root = here::here()) {
  rlang::with_handlers(
    rprojroot::find_package_root_file(path = root),
    error = function(e) NULL
  ) %>%
    is.null() %>%
    magrittr::not()
}

add_trailing_linebreak <- function(x) {
  paste0(x, "\n")
}
