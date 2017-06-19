#' Get the transformer functions for styling
#'
#' @param flat Whether the transformer functions for flat or nested styling
#'   should be returned.
#' @return A list of transformer functions that operate on flat parse tables.
#' @param ... Parameters passed to
#'   * [get_transformers_flat()] if `flat = TRUE` or
#'   * [get_transformers_nested()] if `flat = FALSE`.
#' @export
get_transformers <- function(flat, ...) {
  if (flat) {
    get_transformers_flat(...)
  } else {
    get_transformers_nested(...)
  }
}
#' Get the transformer functions for flat styling
#'
#' @param strict A logical value indicating whether a set of strict
#'   or not so strict transformer functions should be returned.
#' @return A list of transformer functions that operate on flat parse
#'   tables.
#' @export
#' @family obtain transformers
get_transformers_flat <- function(strict = TRUE) {
  c(
    fix_quotes,
    remove_space_before_closing_paren,
    if (strict) remove_space_before_opening_paren,
    add_space_after_for_if_while,
    add_space_before_brace,
    if (strict) set_space_around_op else add_space_around_op,
    if (strict) set_space_after_comma else add_space_after_comma,
    remove_space_after_unary_pm,
    remove_space_after_opening_paren,
    NULL)
}

#' Get the transformer functions for nested styling
#'
#' Similar to [get_transformers_flat()], but additionally, returns some
#'   functions needed due the fact that styling is done in a nested way.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @inheritParams get_transformers_flat
#' @family obtain transformers
#' @importFrom purrr partial
#' @export
get_transformers_nested <- function(strict = TRUE, indent_by = 2) {
  c(create_filler,
    partial(indent_round, indent_by = indent_by),
    get_transformers_flat(strict)
    )
}
