#' Open pre-commit related files
#'
#' @details
#' * `open_config()`: opens the pre-commit config file.
#' * `open_wordlist()`: opens the the WORDLIST file for the check-spelling hook
#'   in inst/WORDLIST.
#' @family helpers
#' @export
open_config <- function() {
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(".pre-commit-config.yaml")
  } else {
    rlang::abort("Can't open if you don't have RStudio running.")
  }
}

#' @export
#' @rdname open_config
open_wordlist <- function() {
  rstudioapi::navigateToFile("inst/WORDLIST")
}
