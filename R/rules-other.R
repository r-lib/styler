#' @importFrom purrr reduce
add_brackets_in_pipe <- function(pd) {
  is_pipe <- pd$token == "SPECIAL-PIPE"
  reduce(which(is_pipe), add_brackets_in_pipe_one, .init = pd)
}

add_brackets_in_pipe_one <- function(pd, pos) {
  next_non_comment <- next_non_comment(pd, pos)
  if (nrow(pd$child[[next_non_comment]]) < 2) {
    new_pos_ids <- create_pos_ids(pd$child[[next_non_comment]], 1, after = TRUE, n = 2)
    new_pd <- create_tokens(
      tokens = c("'('", "')'"), texts = c("(", ")"), pos_ids = new_pos_ids,
      lag_newlines = rep(0, 2))
    pd$child[[next_non_comment]] <-
      bind_rows(pd$child[[next_non_comment]], new_pd) %>%
      arrange(pos_id)
  }
  pd
}

#' Wrap if-else statement in curly braces
#'
#' Wrap an if-else statement in curly braces if it is not already wrapped in
#' a such.
#' @param pd A parse table.
#' @param indent_by The amont of spaces used to indent an expression in curly
#'   braces. Used for unindention.
wrap_if_else_multi_line_in_curly <- function(pd, indent_by = 2) {
  to_be_wrapped_expr_with_child <- next_non_comment(pd, 4)
  if (pd$token[1] == "IF" &&
    pd_is_multi_line(pd) &&
    !is_curly_expr(pd$child[[to_be_wrapped_expr_with_child]])) {

    all_to_be_wrapped_ind <- seq2(5, to_be_wrapped_expr_with_child)
    to_be_wrapped_starts_with_comment <-
      pd[all_to_be_wrapped_ind[1],]$token == "COMMENT"
    pd$spaces[4] <- 1L
    new_expr <- wrap_expr_in_curly(
      pd[all_to_be_wrapped_ind,],
      stretch_out = c(!to_be_wrapped_starts_with_comment, TRUE)
    )
    new_expr$indent <- pd$indent[to_be_wrapped_expr_with_child] - indent_by
    new_expr_in_expr <- new_expr %>%
      wrap_expr_in_expr() %>%
      remove_attributes(c("token_before", "token_after"))

    pd <- pd %>%
      slice(-all_to_be_wrapped_ind) %>%
      bind_rows(new_expr_in_expr) %>%
      set_multi_line() %>%
      arrange(pos_id)

    if (nrow(pd) > 5) pd$lag_newlines[6] <- 0L
  }

  to_be_wrapped_expr_with_child <- next_non_comment(pd, 4)
  after_to_be_wrapped <- next_non_comment(pd, to_be_wrapped_expr_with_child)
  after_else <- next_non_comment(pd, after_to_be_wrapped)

  if (length(after_to_be_wrapped) > 0 &&
     (pd$token[after_to_be_wrapped] == "ELSE" && pd_is_multi_line(pd)) &&
      pd$child[[after_else]]$token != "IF" &&
   !is_curly_expr(pd$child[[after_else]])) {
  pd$spaces[after_to_be_wrapped] <- 1L
  pd$lag_newlines[after_to_be_wrapped] <- 0L
  all_to_be_wrapped_ind <- seq2(after_to_be_wrapped + 1L, after_else)

  new_expr <- wrap_expr_in_curly(
    pd[all_to_be_wrapped_ind,],
    stretch_out = c(TRUE, TRUE)
  )
  new_expr$indent <- pd$indent[after_to_be_wrapped + 1] - indent_by
  pd$spaces[after_else] <- 0L

  new_expr_in_expr <- new_expr %>%
    wrap_expr_in_expr() %>%
    remove_attributes(c("token_before", "token_after"))

  pd <- pd %>%
    slice(-all_to_be_wrapped_ind) %>%
    bind_rows(new_expr_in_expr) %>%
    arrange(pos_id)
   }
  pd
}
