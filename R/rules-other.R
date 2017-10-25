#' @importFrom purrr reduce
add_brackets_in_pipe <- function(pd) {
  is_pipe <- pd$token == "SPECIAL-PIPE"
  if (any(is_pipe, na.rm = TRUE)) {
    pd <- reduce(which(is_pipe), add_brackets_in_pipe_one, .init = pd)
  }
  pd

}

add_brackets_in_pipe_one <- function(pd, pos) {
  next_non_comment <- next_non_comment(pd, pos)
  if (nrow(pd$child[[next_non_comment]]) < 2) {
    new_pos_ids <- create_pos_ids(pd$child[[next_non_comment]], 1, after = TRUE, n = 2)
    new_pd <- create_tokens(
      tokens = c("'('", "')'"), texts = c("(", ")"), pos_ids = new_pos_ids,
      lag_newlines = rep(0, 2), parents = create_parent_id(pd))
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
  if (pd$token[1] == "IF" &&
      pd_is_multi_line(pd) &&
      !is_curly_expr(pd$child[[5]])) {
    pd$lag_newlines[5] <- 0L
    pd$spaces[4] <- 1L
    pd$spaces[5] <- 0L
    if (nrow(pd) > 5) pd$lag_newlines[6] <- 0L
    pd$indent[5] <- pd$indent[5] - indent_by
    pd$child[[5]] <- wrap_expr_in_curly(pd$child[[5]], stretch_out = TRUE)
  }
  if (nrow(pd) > 6 &&
      (pd$token[6] == "ELSE" && pd_is_multi_line(pd)) &&
      pd$child[[7]]$token != "IF" &&
      !is_curly_expr(pd$child[[7]])) {
    pd$spaces[6] <- 1L
    pd$spaces[7] <- 0L
    pd$indent[7] <- pd$indent[7] - indent_by
    pd$lag_newlines[7] <- 0L
    pd$child[[7]] <- wrap_expr_in_curly(pd$child[[7]], stretch_out = TRUE)
  }
  pd
}
