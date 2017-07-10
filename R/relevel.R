#' Flatten some token in the nested parse table based on operators
#'
#' Certain tokens are not placed optimally in the nested parse data with
#'   [compute_parse_data_nested()]. For example, the token of arithmetic
#'   operations 1 + 1 + 1 should all be on the same level of nesting since
#'   the indention is the same for all but the first two terminals. Setting the
#'   indention correcly is easier to achieve if they are put on the same level
#'   of nesting.
#' @param pd_nested A nested parse table to partially flatten.
flatten_operators <- function(pd_nested) {
  pd_nested %>%
    post_visit(c(flatten_operators_one))
}


flatten_operators_one <- function(pd_nested) {
  token <- c("'+'", "'-'", "SPECIAL", "'/'", "'*'")
  token_pos <- which(pd_nested$token %in% token)
  if (length(token_pos) == 0) return(pd_nested)
  stopifnot(length(token_pos) == 1)

  lhs_pos <- token_pos - 1L
  if (lhs_pos < 1) return(pd_nested)
  if (!any(pd_nested$child[[lhs_pos]]$token %in% token)) return(pd_nested)

  pd_nested %>%
    slice(-lhs_pos) %>%
    bind_rows(pd_nested$child[[lhs_pos]]) %>%
    arrange(line1, col1)
}

