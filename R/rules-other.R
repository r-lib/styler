#' @importFrom rlang seq2
add_brackets_in_pipe <- function(pd) {
  has_no_brackets <- (pd$token_before == "SPECIAL-PIPE") &
    (pd$token == "SYMBOL") & (pd$text != ".")
  if (!any(has_no_brackets)) return(pd)
  new_pos_id <- create_pos_id(pd, 1, after = TRUE, n = 2)
  new_pd <- create_tokens(
    tokens = c("'('", "')'"), texts = c("(", ")"), pos_ids = new_pos_id,
    lag_newlines = rep(0, 2), parents = create_parent_id(pd))
  pd <- bind_rows(pd, new_pd)
  pd

}

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
