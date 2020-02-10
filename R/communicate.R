#' Communicate a warning if necessary
#'
#' If round trip verification was not possible, issue a warning to review the
#' changes carefully.
#' @param changed Boolean with indicating for each file whether or not it has
#'   been changed.
#' @inheritParams can_verify_roundtrip
#' @keywords internal
communicate_warning <- function(changed, transformers) {
  if (any(changed, na.rm = TRUE) && !can_verify_roundtrip(transformers)) {
    cat("Please review the changes carefully!", fill = TRUE)
  }
}

#' Communicate the summary of styling
#'
#' @param changed Boolean with indicating for each file whether or not it has
#'   been changed.
#' @param ruler_width Integer used to determine the width of the ruler.
#' @keywords internal
communicate_summary <- function(changed, ruler_width) {
  cli::cat_rule(width = max(40, ruler_width))
  cat("Status\tCount\tLegend \n")
  cli::cat_bullet("\t", sum(!changed, na.rm = TRUE), "\tFile unchanged.", bullet = "tick")
  cli::cat_bullet("\t", sum(changed, na.rm = TRUE), "\tFile changed.", bullet = "info")
  cli::cat_bullet(bullet = "cross", "\t", sum(is.na(changed)), "\tStyling threw an error.")
  cli::cat_rule(width = max(40, ruler_width))
}

#' @importFrom rlang abort
#' @importFrom rlang is_installed
assert_data.tree_installation <- function() {
  if (!is_installed("data.tree")) {
    abort("The package data.tree needs to be installed for this functionality.")
  }
}
