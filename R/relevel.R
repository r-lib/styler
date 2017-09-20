#' Flatten some token in the nested parse table based on operators
#'
#' Certain tokens are not placed optimally in the nested parse data with
#'   [compute_parse_data_nested()]. For example, the token of arithmetic
#'   operations 1 + 1 + 1 should all be on the same level of nesting since
#'   the indention is the same for all but the first two terminals. Setting the
#'   indention correctly is easier to achieve if they are put on the same level
#'   of nesting.
#' @param pd_nested A nested parse table to partially flatten.
flatten_operators <- function(pd_nested) {
  pd_nested %>%
    post_visit(c(flatten_operators_one))
}

#' Flatten one level of nesting with its child
#'
#' Flattening is done in two ways. We can flatten a parse table by moving
#'   the left hand token of an operator one level up. Or doing that with the
#'   right hand token.
#' @param pd_nested A nested parse table.
#' @include token-define.R
flatten_operators_one <- function(pd_nested) {
  pd_token_left <- c(special_token, math_token, "'$'")
  pd_token_right <- c(special_token, "LEFT_ASSIGN",  "'+'", "'-'")
  bound <- pd_nested %>%
    flatten_pd(pd_token_left, left = TRUE) %>%
    flatten_pd(pd_token_right, left = FALSE)
  bound
}


#' Flatten a parse table
#'
#' Flattens a parse table if certain tokens occur in this table or its child,
#' either flattening from left or from right. If one of `token` is present in
#' `pd_nested`  and one of `child_token` is present in one of the children next
#' to `token` in `pd_nested`, the nested parse table is flattened. Otherwise, it
#' is returned unmodified.
#' @param pd_nested A nested parse table.
#' @param token A character vector with tokens of which at least one has to
#'   occur in `pd_nested` in order to flatten it.
#' @param child_token A character vector of tokens of which at least one has to
#'   occur in the child in order to flatten the parse table.
#' @param left Flag that indicates whether the parse table should be flattened
#'   from left or from right.
flatten_pd <- function(pd_nested, token, child_token = token, left = TRUE) {
  token_pos <- which(pd_nested$token[-1] %in% token) + 1
  if (length(token_pos) == 0) return(pd_nested)
  pos <- token_pos[if_else(left, 1, length(token_pos))] + if_else(left, -1L, 1L)
  if (pos < 1) return(pd_nested)
  if (!any(pd_nested$child[[pos]]$token[-1] %in% child_token)) return(pd_nested)
  bind_with_child(pd_nested, pos)
}

#' Bind a parse table with one of its children
#'
#' Bind a parse table with one of its children and return parse table, ordered
#' according to the appearance of the tokens.
#' @param pd_nested A nested parse table.
#' @param pos The position of the child to bind.
bind_with_child <- function(pd_nested, pos) {
  pd_nested %>%
    slice(-pos) %>%
    bind_rows(pd_nested$child[[pos]]) %>%
    arrange(pos_id)
}

