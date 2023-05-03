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
  pd <- vec_slice(pd, !is_semicolon)
  pd
}

add_brackets_in_pipe <- function(pd) {
  if (!identical(pd$text[next_non_comment(pd, 0L)], "substitute")) {
    pd$child <- map(pd$child, add_brackets_in_pipe_child)
  }
  pd
}

add_brackets_in_pipe_child <- function(pd) {
  is_pipe <- pd$token %in% c("SPECIAL-PIPE", "PIPE")
  Reduce(add_brackets_in_pipe_one, which(is_pipe), init = pd)
}

add_brackets_in_pipe_one <- function(pd, pos) {
  next_non_comment <- next_non_comment(pd, pos)
  rh_child <- pd$child[[next_non_comment]]
  if (nrow(rh_child) < 2L && rh_child$token == "SYMBOL") {
    child <- pd$child[[next_non_comment]]
    new_pos_ids <- create_pos_ids(child, 1L, after = TRUE, n = 2L)
    new_pd <- create_tokens(
      texts = c("(", ")"),
      lag_newlines = rep(0L, 2L),
      spaces = 0L,
      pos_ids = new_pos_ids,
      token_before = c(child$token[1L], "'('"),
      token_after = c("')'", child$token_after[1L]),
      indention_ref_pos_ids = NA,
      indents = child$indent[1L],
      tokens = c("'('", "')'"),
      terminal = TRUE,
      child = NULL,
      stylerignore = child$stylerignore[1L],
      # block???
      block = NA,
      is_cached = FALSE
    )
    pd$child[[next_non_comment]] <- vec_rbind(pd$child[[next_non_comment]], new_pd) %>%
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
wrap_if_else_while_for_fun_multi_line_in_curly <- function(pd, indent_by = 2L) {
  key_token <- NULL

  if (is_for_expr(pd)) {
    key_token <- "forcond"
  } else if (is_conditional_expr(pd) || is_while_expr(pd) || is_function_declaration(pd)) {
    key_token <- "')'"
  }

  if (length(key_token) > 0L) {
    pd <- pd %>%
      wrap_multiline_curly(indent_by,
        key_token = key_token,
        space_after = as.integer(contains_else_expr(pd))
      )
  }
  if (is_conditional_expr(pd)) {
    pd <- pd %>%
      wrap_else_multiline_curly(indent_by, space_after = 0L)
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
wrap_multiline_curly <- function(pd, indent_by, key_token, space_after = 1L) {
  to_be_wrapped_expr_with_child <- next_non_comment(
    pd, which(pd$token == key_token)[1L]
  )
  next_terminal <- next_terminal(vec_slice(pd, to_be_wrapped_expr_with_child))$text
  requires_braces <- if_for_while_part_requires_braces(pd, key_token) && !any(pd$stylerignore)
  if (requires_braces || next_terminal == "return") {
    closing_brace_ind <- which(pd$token == key_token)[1L]
    pd$spaces[closing_brace_ind] <- 1L

    all_to_be_wrapped_ind <- seq2(
      closing_brace_ind + 1L, to_be_wrapped_expr_with_child
    )

    pd <- wrap_subexpr_in_curly(
      pd, all_to_be_wrapped_ind, indent_by, space_after
    )

    if (nrow(pd) > 5L) pd$lag_newlines[6L] <- 0L
  }
  pd
}

#' Add curly braces to else
#'
#' Wrap the else part of a conditional expression into curly braces if not
#' already wrapped into a such.
#' @inheritParams wrap_multiline_curly
#' @keywords internal
wrap_else_multiline_curly <- function(pd, indent_by = 2L, space_after = 0L) {
  if (contains_else_expr(pd) &&
    pd_is_multi_line(pd) &&
    contains_else_expr_that_needs_braces(pd) &&
    !any(pd$stylerignore) &&
    pd$token_before[1L] != "SPECIAL-PIPE") {
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
    pd$token[ind_to_be_wrapped[1L]] == "COMMENT"
  new_expr <- wrap_expr_in_curly(
    vec_slice(pd, ind_to_be_wrapped),
    stretch_out = c(!to_be_wrapped_starts_with_comment, TRUE),
    space_after = space_after
  )
  new_expr$indent <- max(pd$indent[last(ind_to_be_wrapped)] - indent_by, 0L)
  new_expr_in_expr <- new_expr %>%
    wrap_expr_in_expr() %>%
    remove_attributes(c("token_before", "token_after"))

  pd %>%
    vec_slice(-ind_to_be_wrapped) %>%
    vec_rbind(new_expr_in_expr) %>%
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
  pos_first_key_token <- which(pd$token == key_token)[1L]
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
#' @param pd_flat A flat parse table.
#' @keywords internal
fix_quotes <- function(pd_flat) {
  str_const <- which(pd_flat$token == "STR_CONST")
  if (rlang::is_empty(str_const)) {
    return(pd_flat)
  }

  pd_flat$text[str_const] <- map_chr(pd_flat$text[str_const], fix_quotes_one)
  pd_flat
}

fix_quotes_one <- function(x) {
  rx <- "^'([^\"]*)'$"
  i <- grep(rx, x)
  if (rlang::is_empty(i)) {
    return(x)
  }

  # replace outer single quotes
  xi <- gsub(rx, '"\\1"', x[i])

  # Replace inner escaped quotes (\') by ' and keep all other instances of \., including \\
  x[i] <- gsub("\\\\(')|(\\\\[^'])", "\\1\\2", xi)
  x
}
