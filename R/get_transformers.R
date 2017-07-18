#' Get the transformer functions for styling
#'
#' @param flat Whether the transformer functions for flat or nested styling
#'   should be returned.
#' @return A list of transformer functions that operate on flat parse tables.
#' @param ... Parameters passed to
#'   * [get_transformers_flat()] if `flat = TRUE` or
#'   * [get_transformers_nested()] if `flat = FALSE`.
#' @export
get_transformers <- function(flat = FALSE, ...) {
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
#' @param start_comments_with_one_space Whether or not comments should start
#'   with only one space (see [start_comments_with_space()]).
#' @return A list of transformer functions that operate on flat parse
#'   tables.
#' @export
#' @family obtain transformers
#' @importFrom purrr partial
get_transformers_flat <- function(strict = TRUE,
                                  start_comments_with_one_space = FALSE) {
  c(
    fix_quotes,
    remove_space_before_closing_paren,
    if (strict) remove_space_before_opening_paren,
    add_space_after_for_if_while,
    add_space_before_brace,
    if (strict) set_space_around_op else add_space_around_op,
    if (strict) set_space_after_comma else add_space_after_comma,
    remove_space_before_comma,
    remove_space_after_opening_paren,
    partial(start_comments_with_space,
            force_one = start_comments_with_one_space),
    NULL)
}

#' Get the transformer functions for nested styling
#'
#' Similar to [get_transformers_flat()], but additionally, returns some
#'   functions needed due the fact that styling is done in a nested way.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @param manipulate_spaces Whether spaces should be manipulated in the styling.
#' @param manipulate_line_breaks Whether line breaks should be manipulated in
#'   the styling.
#' @param manipulate_tokens Whether tokens should be manipulated in the styling.
#' @inheritParams get_transformers_flat
#' @family obtain transformers
#' @importFrom purrr partial
#' @export
get_transformers_nested <- function(
  manipulate_spaces = TRUE,
  manipulate_line_breaks = TRUE,
  manipulate_tokens = TRUE,
  strict = TRUE,
  indent_by = 2,
  start_comments_with_one_space = FALSE) {

  space_manipulators <- if (manipulate_line_breaks)
    c(
      partial(indent_round, indent_by = indent_by),
      partial(indent_curly, indent_by = indent_by),
      partial(indent_op, indent_by = indent_by),
      partial(indent_without_paren, indent_by = indent_by),
      get_transformers_flat(strict, start_comments_with_one_space),
      remove_space_after_unary_pm_nested,
      set_space_before_comments,
      set_space_between_levels
    )

  line_break_manipulators <- if (manipulate_line_breaks)
    c(
      remove_line_break_before_curly_opening,
      remove_line_break_before_round_closing,
      add_line_break_afer_curly_opening,
      add_line_break_before_curly_closing,
      add_line_break_after_pipe
    )

  token_manipulators <- if (manipulate_tokens)
    c(
      force_assignment_op,
      resolve_semicolon,
      add_brackets_in_pipe
    )


  list(
    filler     = create_filler,
    line_break = line_break_manipulators,
    space      = space_manipulators,
    token      = token_manipulators,
    eol        = strip_eol_spaces,
    NULL
  )
}
