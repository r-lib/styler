force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}


resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) {
    return(pd)
  }
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}

add_brackets_in_pipe <- function(pd) {
  is_pipe <- pd$token == "SPECIAL-PIPE"
  Reduce(add_brackets_in_pipe_one, which(is_pipe), init = pd)
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
      arrange_pos_id()
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
wrap_if_else_while_for_fun_multi_line_in_curly <- function(pd, indent_by = 2) {
  key_token <- when(
    pd,
    is_cond_expr(.) ~ "')'",
    is_while_expr(.) ~ "')'",
    is_for_expr(.) ~ "forcond",
    is_function_dec(.) ~ "')'"
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
#' @inheritParams wrap_if_else_while_for_fun_multi_line_in_curly
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
  requires_braces <- if_for_while_part_requires_braces(pd, key_token) && !any(pd$stylerignore)
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
#' already wrapped into a such.
#' @inheritParams wrap_multiline_curly
#' @keywords internal
wrap_else_multiline_curly <- function(pd, indent_by = 2, space_after = 0) {
  if (contains_else_expr(pd) &&
    pd_is_multi_line(pd) &&
    contains_else_expr_that_needs_braces(pd) &&
    !any(pd$stylerignore)) {
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
  new_expr$indent <- max(pd$indent[last(ind_to_be_wrapped)] - indent_by, 0)
  new_expr_in_expr <- new_expr %>%
    wrap_expr_in_expr() %>%
    remove_attributes(c("token_before", "token_after"))

  pd %>%
    slice(-ind_to_be_wrapped) %>%
    bind_rows(new_expr_in_expr) %>%
    set_multi_line() %>%
    arrange_pos_id()
}

#' Check if if, for or while loop expression require a braces.
#'
#' This is the case if they are multi-line and not yet wrapped into curly
#' braces.
#' @inheritParams wrap_multiline_curly
#' @keywords internal
if_for_while_part_requires_braces <- function(pd, key_token) {
  pos_first_key_token <- which(pd$token == key_token)[1]
  child <- pd$child[[next_non_comment(pd, pos_first_key_token)]]
  pd_is_multi_line(pd) && !is_curly_expr(child)
}

#' Replace single quotes with double quotes
#'
#' We do not use `deparse()` as in previous implementations but `paste0()` since
#' the former approach escapes the reverse backslash in the line break character
#' `\\n` whereas the solution with `paste0()` does not.
#' @examples
#' style_text("'here
#' is a string
#' '")
#' @importFrom purrr map map_chr
#' @param pd_flat A flat parse table.
#' @importFrom rlang is_empty
#' @keywords internal
fix_quotes <- function(pd_flat) {
  str_const <- which(pd_flat$token == "STR_CONST")
  if (is_empty(str_const)) {
    return(pd_flat)
  }

  pd_flat$text[str_const] <- map(pd_flat$text[str_const], fix_quotes_one)
  pd_flat
}

#' @importFrom rlang is_empty
fix_quotes_one <- function(x) {
  rx <- "^'([^\"]*)'$"
  i <- grep(rx, x)
  if (is_empty(i)) {
    return(x)
  }

  # replace outer single quotes
  xi <- gsub(rx, '"\\1"', x[i])

  # Replace inner escaped quotes (\') by ' and keep all other instances of \., including \\
  x[i] <- gsub("\\\\(')|(\\\\[^'])", "\\1\\2", xi)
  x
}
