#' Style guides
#'
#' Format code according to a style guide. Style guides are the input to the
#' argument `style` in [style_file()] and friends.
#' The available style guides are:
#' * the tidyverse style guide (see [tidyverse_style()]).
#' @name style_guides
NULL


#' The tidyverse style
#'
#' Style code according to the tidyverse style guide.
#' @param scope The extent of manipulation. Can range from "none" (least
#'   invasive) to "token" (most invasive). See 'Details'. This argument is a
#'   vector of length one.
#' @param indent_by How many spaces of indention should be inserted after
#'   operators such as '('.
#' @param strict A logical value indicating whether a set of strict
#'   or not so strict transformer functions should be returned. Compare the
#'   functions returned with or without `strict = TRUE`. For example,
#'   `strict = TRUE` means force *one* space e.g. after "," and *one* line break
#'   e.g. after a closing curly brace. `strict = FALSE` means to set spaces and
#'   line breaks to one if there is none and leave the code untouched otherwise.
#'   See 'Examples'.
#' @param start_comments_with_one_space Whether or not comments should start
#'   with only one space (see [start_comments_with_space()]).
#' @inheritParams create_style_guide
#' @param math_token_spacing A list of parameters that define spacing around
#'   math token, conveniently constructed using [specify_math_token_spacing()].

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
#' @family style_guides
#' @examples
#' style_text("call( 1)", style = tidyverse_style, scope = "spaces")
#' style_text(c("ab <- 3", "a  <-3"), strict = FALSE) # keeps alignment of "<-"
#' style_text(c("ab <- 3", "a  <-3"), strict = TRUE) # drops alignment of "<-"
#' @importFrom purrr partial
#' @export
tidyverse_style <- function(scope = "tokens",
                            strict = TRUE,
                            indent_by = 2,
                            start_comments_with_one_space = FALSE,
                            reindention = tidyverse_reindention(),
                            math_token_spacing = tidyverse_math_token_spacing()) {

  scope <- character_to_ordered(
    scope,
    c("none", "spaces", "indention", "line_breaks", "tokens")
  )

  space_manipulators <- if (scope >= "spaces")
    lst(
      partial(indent_braces, indent_by = indent_by),
      partial(indent_op, indent_by = indent_by),
      partial(indent_eq_sub, indent_by = indent_by),
      partial(indent_without_paren, indent_by = indent_by),

      fix_quotes,
      remove_space_before_closing_paren,
      if (strict) remove_space_before_opening_paren else identity,
      add_space_after_for_if_while,
      add_space_before_brace,
      remove_space_before_comma,
      partial(
        style_space_around_math_token, strict,
        math_token_spacing$zero,
        math_token_spacing$one
      ),
      if (strict) set_space_around_op else add_space_around_op,
      if (strict) set_space_after_comma else add_space_after_comma,
      remove_space_after_opening_paren,
      remove_space_after_excl,
      set_space_after_bang_bang,
      remove_space_before_dollar,
      remove_space_around_colons,
      partial(start_comments_with_space,
              force_one = start_comments_with_one_space),

      remove_space_after_unary_pm_nested,
      if (strict) set_space_before_comments else add_space_before_comments,
      set_space_between_levels,
      set_space_between_eq_sub_and_comma
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
      if (strict) partial(
        set_line_break_after_opening_if_call_is_multi_line,
        except_token_after = "COMMENT",
        except_text_before = c("switch", "ifelse", "if_else")
      ) else identity,
      if (strict) partial(
        set_line_break_before_closing_call, except_token_before = "COMMENT"
      ) else identity,
      remove_line_break_in_empty_fun_call,
      add_line_break_after_pipe
    )

  token_manipulators <- if (scope >= "tokens")
    lst(
      force_assignment_op,
      resolve_semicolon,
      add_brackets_in_pipe,
      remove_terminal_token_before_and_after,
      if (strict) wrap_if_else_multi_line_in_curly else identity
    )


  indention_modifier <-
    c(
      update_indention_ref_fun_dec,
      NULL
    )

  create_style_guide(
    # transformer functions
    initialize        = initialize_attributes,
    line_break        = line_break_manipulators,
    space             = space_manipulators,
    token             = token_manipulators,
    indention         = indention_modifier,
    # transformer options
    use_raw_indention = use_raw_indention,
    reindention       = reindention
  )
}

#' Create a style guide
#'
#' This is a helper function to create a style guide, which is technically
#' speaking a named list of groups of transformer functions where each
#' transformer function corresponds to one styling rule. The output of this
#' function can be used as an argument for \code{style} in top level functions
#' like [style_text()] and friends.
#' @param initialize A function that initializes various variables on each
#'   level of nesting.
#' @param line_break A list of transformer functions that manipulate line_break
#'   information.
#' @param space A list of transformer functions that manipulate spacing
#'   information.
#' @param token A list of transformer functions that manipulate token text.
#' @param indention A list of transformer functions that manipulate indention.
#' @param use_raw_indention Boolean indicating whether or not the raw indention
#'   should be used.
#' @param reindention A list of parameters for regex re-indention, most
#'   conveniently constructed using [specify_reindention()].
#' @export
create_style_guide <- function(initialize = initialize_attributes,
                               line_break = NULL,
                               space = NULL,
                               token = NULL,
                               indention = NULL,
                               use_raw_indention = FALSE,
                               reindention = tidyverse_reindention()) {
  lst(
    # transformer functions
    initialize,
    line_break,
    space,
    token,
    indention,
    # transformer options
    use_raw_indention,
    reindention
  )
}


#' Specify what is re-indented how
#'
#' This function returns a list that can be used as an input for the argument
#' `reindention` of the function [tidyverse_style()]. It features sensible
#' defaults, so the user can specify deviations from them conveniently without
#' the need of setting all arguments explicitly.
#' @param regex_pattern Character vector with regular expression patterns that
#'   are to be re-indented with spaces.
#' @param indention The indention tokens should have if they match
#'   `regex_pattern`.
#' @param comments_only Whether the `regex_reindention_pattern` should only be
#'   matched against comments or against all tokens. Mainly added for
#'   performance.
#' @name reindention
NULL

#' @describeIn reindention Allows to specify which tokens are reindented and
#'   how.
#' @export
specify_reindention <- function(regex_pattern = regex_none(),
                                indention = 0,
                                comments_only = TRUE)
  lst(
    regex_pattern,
    indention,
    comments_only
)

#' @describeIn reindention Simple forwarder to
#' `specify_reindention` with reindention according to the tidyverse style
#' guide.
#' @export
tidyverse_reindention <- function() {
  specify_reindention(
    regex_pattern = regex_none(), indention = 0, comments_only = TRUE
  )
}

#' Convert a character vector to an ordered factor
#'
#' Convert a vector to an ordered factor but stop if any of the values in
#'   `x` does not match the predefined levels in `levels.`
#' @param x A character vector.
#' @param levels A vector with levels.
#' @param name The name of the character vector to be displayed if the
#'   construction of the factor fails.
character_to_ordered <- function(x, levels, name = substitute(x)) {
  if (!all((x %in% levels))) {
    stop("all values in ", name, " must be one of the following: ",
         paste(levels, collapse = ", "))
  }
  factor(x, levels = levels, ordered = TRUE)
}

#' Specify spacing around math tokens
#'
#' Helper function to create the input for the argument `math_token_spacing`  in
#' [tidyverse_style()].
#' @inheritParams style_space_around_math_token
#' @examples
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'"),
#'   strict = FALSE
#' )
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'"),
#'   strict = TRUE
#' )
#' style_text(
#'   "1+1   -3",
#'   math_token_spacing = tidyverse_math_token_spacing(),
#'   strict = TRUE
#' )
#' @name math_token_spacing
NULL

#' @describeIn math_token_spacing Allows to fully specify the math token
#'   spacing.
#' @export
specify_math_token_spacing <-
  function(zero = NULL,
           one = c("'+'", "'-'", "'*'", "'/'", "'^'")) {
    assert_tokens(c(one, zero))
    lst(
      one = setdiff(c(math_token, one), zero),
      zero
    )
  }

#' @describeIn math_token_spacing Simple forwarder to
#' `specify_math_token_spacing` with spacing around math tokens according to the
#' tidyverse style guide.
#' @export
tidyverse_math_token_spacing <- function() {
  specify_math_token_spacing(one = c("'+'", "'-'", "'*'", "'/'", "'^'"))
}

#' Check token validity
#'
#' Check whether one or more tokens exist and have a unique token-text mapping
#' @param tokens Tokens to check.
assert_tokens <- function(tokens) {
  invalid_tokens <- tokens[!(tokens %in% lookup_tokens()$token)]
  if (length(invalid_tokens) > 0) {
    stop(
      "Token(s) ", paste0(invalid_tokens, collapse = ", "), " are invalid. ",
      "You can lookup all valid tokens and their text ",
      "with styler:::looup_tokens(). Make sure you supply the values of ",
      "the column 'token', not 'text'."
    )
  }
}
