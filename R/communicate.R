#' Communicate a warning if necessary
#'
#' If round trip verification was not possible, issue a warning to review the
#' changes carefully.
#' @param changed Boolean with indicating for each file whether or not it has
#'   been changed.
#' @inheritParams parse_tree_must_be_identical
#' @keywords internal
communicate_warning <- function(changed, transformers) {
  if (any(changed, na.rm = TRUE) &&
    !parse_tree_must_be_identical(transformers) &&
    !getOption("styler.quiet", FALSE)
  ) {
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
  if (!getOption("styler.quiet", FALSE)) {
    cli::cat_rule(width = max(40L, ruler_width))
    cat("Status\tCount\tLegend \n")
    cli::cat_bullet(
      "\t", sum(!changed, na.rm = TRUE), "\tFile unchanged.",
      bullet = "tick",
      bullet_col = "green"
    )
    cli::cat_bullet(
      "\t", sum(changed, na.rm = TRUE), "\tFile changed.",
      bullet = "info",
      bullet_col = "cyan"
    )
    cli::cat_bullet(
      bullet = "cross", "\t", sum(is.na(changed)), "\tStyling threw an error.",
      bullet_col = "red"
    )
    cli::cat_rule(width = max(40L, ruler_width))
  }
}
