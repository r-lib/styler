#' The tidyverse style
#' @param scope The extent of manipulation. Can range from "none" (least
#'   invasive) to "token" (most invasive). See 'Details'. This argument is a
#'   vector of length one.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @param strict A logical value indicating whether a set of strict
#'   or not so strict transformer functions should be returned.
#' @param start_comments_with_one_space Whether or not comments should start
#'   with only one space (see [start_comments_with_space()]).
#' @details The following options for `scope` are available.
#'
#' * "none": Performs no transformation at all.
#' * "spaces": Manipulates spacing between token on the same line.
#' * "indention": In addition to "spaces", this option also manipulates the
#'   indention level.
#' * "line_breaks": In addition to "indention", this option also manipulates
#'   line breaks.
#' * "tokens": In addition to "line_breaks", this option also manipulates
#'   tokens.
#'
#' As it becomes clear from this description, more invasive operations can only
#' be performed if all less invasive operations are performed too.
#' @family obtain transformers
#' @importFrom purrr partial
#' @export
tidyverse_style <- function(scope = "tokens",
                            strict = TRUE,
                            indent_by = 2,
                            start_comments_with_one_space = FALSE) {

  scope <- character_to_ordered(
    scope,
    c("none", "spaces", "indention", "line_breaks", "tokens")
  )

  space_manipulators <- if (scope >= "spaces")
    lst(
      partial(indent_round, indent_by = indent_by),
      partial(indent_curly, indent_by = indent_by),
      partial(indent_op, indent_by = indent_by),
      partial(indent_eq_sub, indent_by = indent_by),
      partial(indent_without_paren, indent_by = indent_by),

      fix_quotes,
      remove_space_before_closing_paren,
      if (strict) remove_space_before_opening_paren else identity,
      add_space_after_for_if_while,
      add_space_before_brace,
      if (strict) set_space_around_op else add_space_around_op,
      if (strict) set_space_after_comma else add_space_after_comma,
      remove_space_before_comma,
      remove_space_after_opening_paren,
      remove_space_after_excl,
      remove_space_before_dollar,
      remove_space_around_colons,
      partial(start_comments_with_space,
              force_one = start_comments_with_one_space),

      remove_space_after_unary_pm_nested,
      set_space_before_comments,
      set_space_between_levels,
      set_space_between_eq_sub_and_comma,
    )

  use_raw_indention <- scope < "indention"

  line_break_manipulators <- if (scope >= "line_breaks")
    lst(
      remove_line_break_before_curly_opening,
      remove_line_break_before_round_closing,
      if (strict) set_line_break_afer_curly_opening else
        add_line_break_afer_curly_opening,
      if (strict) set_line_break_before_curly_closing else
        add_line_break_before_curly_closing,
      add_line_break_after_pipe
    )

  token_manipulators <- if (scope >= "tokens")
    lst(
      force_assignment_op,
      resolve_semicolon,
      add_brackets_in_pipe
    )


  indention_modifier <-
    c(
      update_indention_ref_fun_dec,
      update_indention_ref_fun_call
    )

  create_style_guide(
    # transformer functions
    filler            = create_filler,
    line_break        = line_break_manipulators,
    space             = space_manipulators,
    token             = token_manipulators,
    indention         = indention_modifier,
    # transformer options
    use_raw_indention = use_raw_indention
  )
}

#' Create a style guide
#'
#' This is a helper function to create a style guide, which is technically
#' speaking a named list of groups of transformer functions where each
#' transformer function corresponds to one styling rule. The output of this
#' function can be used as an argument for \code{style} in top level functions
#' like [style_text()] and friends.
#' @param filler A filler function that initializes various variables on each
#'   level of nesting.
#' @param line_break A list of transformer functiosn that manipulate line_break
#'   information.
#' @param space A list of transformer functions that manipulate spacing
#'   information.
#' @param token A list of transformer functions that manipulate token text.
#' @param indention A list of transformer functions that manipulate indention.
#' @param use_raw_indention Boolean indicating wheter or not the raw indention
#'   should be used.
#' @export
create_style_guide <- function(filler = create_filler,
                               line_break = NULL,
                               space = NULL,
                               token = NULL,
                               indention = NULL,
                               use_raw_indention = FALSE) {
  lst(
    # transformer functions
    filler,
    line_break,
    space,
    token,
    indention,
    # transformer options
    use_raw_indention
  )
}

#' Convert a character vector to an ordered factor
#'
#' Convert a vector to an ordered factor but stop if any of the values in
#'   `x` does not match the predefined levels in `levels.`
#' @param x A character vector.
#' @param levels A vector with levels.
#' @param name The name of the character vector to be dispayed if the
#'   construction of the factor fails.
character_to_ordered <- function(x, levels, name = substitute(x)) {
  if (!all((x %in% levels))) {
    stop("all values in ", name, " must be one of the following: ",
         paste(levels, collapse = ", "))
  }
  factor(x, levels = levels, ordered = TRUE)
}

