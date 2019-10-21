#' Open pre-commit related files
#'
#' @details
#' * `open_config()`: opens the pre-commit config file.
#' * `open_wordlist()`: opens the the WORDLIST file for the check-spelling hook
#'   in inst/WORDLIST.
#' @inheritParams fallback_doc
#' @family helpers
#' @export
open_config <- function(path_root = here::here()) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(fs::path(path_root, ".pre-commit-config.yaml"))
  } else {
    rlang::abort("Can't open if you don't have RStudio running.")
  }
}

#' @export
#' @rdname open_config
open_wordlist <- function(path_root = here::here()) {
  rstudioapi::navigateToFile(fs::path(path_root, "inst", "WORDLIST"))
}
