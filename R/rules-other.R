#' @importFrom purrr reduce
add_brackets_in_pipe <- function(pd) {
  is_pipe <- pd$token == "SPECIAL-PIPE"
  reduce(which(is_pipe), add_brackets_in_pipe_one, .init = pd)
}

add_brackets_in_pipe_one <- function(pd, pos) {
  next_non_comment <- next_non_comment(pd, pos)
  rh_child <- pd$child[[next_non_comment]]
  if (nrow(rh_child) < 2 && rh_child$token == "SYMBOL") {
    child <- pd$child[[next_non_comment]]
    new_pos_ids <- create_pos_ids(child, 1, after = TRUE, n = 2)
    new_pd <- create_tokens(
      tokens = c("'('", "')'"),
      texts = c("(", ")"),
      pos_ids = new_pos_ids,
      lag_newlines = rep(0, 2)
    )
    pd$child[[next_non_comment]] <- bind_rows(
      pd$child[[next_non_comment]],
      new_pd
    ) %>%
      arrange(pos_id)
  }
  pd
}

#' Wrap if-else, while and for statements in curly braces
#'
#' Wrap statements in curly braces if it is not already wrapped in a such.
#' @param pd A parse table.
#' @param indent_by The amount of spaces used to indent an expression in curly
#'   braces. Used for unindention.
#' @keywords internal
#' @importFrom purrr when
wrap_if_else_while_for_multi_line_in_curly <- function(pd, indent_by = 2) {
  key_token <- when(
    pd,
    is_cond_expr(.) ~ "')'",
    is_while_expr(.) ~ "')'",
    is_for_expr(.) ~ "forcond"
  )
  if (length(key_token) > 0) {
    pd <- pd %>%
      wrap_multiline_curly(indent_by,
        space_after = ifelse(contains_else_expr(pd), 1, 0),
        key_token = key_token
      )
  }
  if (is_cond_expr(pd)) {
    pd <- pd %>%
      wrap_else_multiline_curly(indent_by, space_after = 0)
  }
  pd
}

#' Wrap a multi-line statement in curly braces
#'
#' @inheritParams wrap_if_else_while_for_multi_line_in_curly
#' @inheritParams wrap_subexpr_in_curly
#' @param key_token The token that comes right before the token that contains
#'   the expression to be wrapped (ignoring comments). For if and while loops,
#'   this is the closing "')'", for a for-loop it's "forcond".
#' @keywords internal
wrap_multiline_curly <- function(pd, indent_by, space_after = 1, key_token) {
  to_be_wrapped_expr_with_child <- next_non_comment(
    pd, which(pd$token == key_token)[1]
  )
  next_terminal <- next_terminal(pd[to_be_wrapped_expr_with_child, ])$text
  requires_braces <- if_for_while_part_requires_braces(pd, key_token)
  if (requires_braces | next_terminal == "return") {
    closing_brace_ind <- which(pd$token == key_token)[1]
    pd$spaces[closing_brace_ind] <- 1L

    all_to_be_wrapped_ind <- seq2(
      closing_brace_ind + 1L, to_be_wrapped_expr_with_child
    )

    pd <- wrap_subexpr_in_curly(
      pd, all_to_be_wrapped_ind, indent_by, space_after
    )

    if (nrow(pd) > 5) pd$lag_newlines[6] <- 0L
  }
  pd
}

#' Add curly braces to else
#'
#' Wrap the else part of a conditional expression into curly braces if not
#' already wraped into a such.
#' @inheritParams wrap_multiline_curly
#' @keywords internal
wrap_else_multiline_curly <- function(pd, indent_by = 2, space_after = 0) {
  if (contains_else_expr(pd) &&
    pd_is_multi_line(pd) &&
    contains_else_expr_that_needs_braces(pd)) {
    else_idx <- which(pd$token == "ELSE")
    pd$spaces[else_idx] <- 1L
    all_to_be_wrapped_ind <- seq2(else_idx + 1L, nrow(pd))

    pd <- wrap_subexpr_in_curly(
      pd, all_to_be_wrapped_ind, indent_by, space_after
    )
  }
  pd
}

#' Wrap a sub-expression in curly braces
#'
#' Wraps some rows of a parse table into a sub-expression.
#' @inheritParams wrap_multiline_curly
#' @param ind_to_be_wrapped The indices of the rows that should be wrapped
#'   into a new expression.
#' @inheritParams wrap_expr_in_curly
#' @keywords internal
wrap_subexpr_in_curly <- function(pd,
                                  ind_to_be_wrapped,
                                  indent_by,
                                  space_after) {
  to_be_wrapped_starts_with_comment <-
    pd$token[ind_to_be_wrapped[1]] == "COMMENT"
  new_expr <- wrap_expr_in_curly(
    pd[ind_to_be_wrapped, ],
    stretch_out = c(!to_be_wrapped_starts_with_comment, TRUE),
    space_after = space_after
  )
  new_expr$indent <- pd$indent[last(ind_to_be_wrapped)] - indent_by
  new_expr_in_expr <- new_expr %>%
    wrap_expr_in_expr() %>%
    remove_attributes(c("token_before", "token_after"))

  pd %>%
    slice(-ind_to_be_wrapped) %>%
    bind_rows(new_expr_in_expr) %>%
    set_multi_line() %>%
    arrange(pos_id)
}

#' Check if if, for or while loop expression require a braces.
#'
#' This is the case if they are multi-line and not yet wraped into curly
#' braces.
#' @inheritParams wrap_multiline_curly
#' @keywords internal
if_for_while_part_requires_braces <- function(pd, key_token) {
  pos_first_key_token <- which(pd$token == key_token)[1]
  child <- pd$child[[next_non_comment(pd, pos_first_key_token)]]
  pd_is_multi_line(pd) && !is_curly_expr(child)
}
