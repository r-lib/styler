#' get the transformer functions
#'
#' @param strict A logical value indicating whether a set of strict
#'   or not so strict transformer functions should be returned.
#' @return A list of transformer functions that operate on parse
#'   tables.
#' @export
get_transformers <- function(strict = TRUE) {
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
