#' Open pre-commit related files
#'
#' @details
#' * `open_config()`: opens the pre-commit config file.
#' * `open_wordlist()`: opens the the WORDLIST file for the check-spelling hook
#'   in inst/WORDLIST.
#' @inheritParams fallback_doc
#' @return
#' `NULL` (invisibly). The function is called for its side effects.
#' @family helpers
#' @export
open_config <- function(root = here::here()) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(fs::path(root, ".pre-commit-config.yaml"))
  } else {
    rlang::abort("Can't open if you don't have RStudio running.")
  }
  invisible(NULL)
}

#' @export
#' @rdname open_config
open_wordlist <- function(root = here::here()) {
  rstudioapi::navigateToFile(fs::path(root, "inst", "WORDLIST"))
  invisible(NULL)
}
