#' Reorganize a nested parse table
#'
#' Certain tokens are not placed optimally in the nested parse data with
#'   [compute_parse_data_nested()]. For example, the token of arithmetic
#'   operations 1 + 1 + 1 should all be on the same level of nesting since
#'   the indention is the same for all but the first two terminals. Setting the
#'   indention correcly is easier to achieve if they are put on the same level
#'   of nesting.
#' @examples
#' code <- "1+1+1+1"
#' # tree-structure with compute_parse_data_nested
#' styler:::create_tree(code, re_nest = FALSE)
#' # tree-structure with re_nest post-processing
#' styler:::create_tree(code, re_nest = TRUE)
#' @inheritParams move_children_up
re_nest <- function(pd_nested) {
  if (is.null(pd_nested)) return()
  pd_nested <- pd_nested %>%
    move_children_up(token = c("'+'", "'-'", "SPECIAL", "'/'", "'*'")) %>%
    mutate(child = map(child, re_nest))
  pd_nested
}

re_nest2 <- function(pd_nested) {
  pd_nested %>%
    visit_post(c(re_nest2_one))
}

re_nest2_one <- function(pd_nested) {
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

#' Move children up if appropriate
#'
#' Move children one level of nesting up if necessary. If any was moved up,
#'   check again whether any child of the former child should be moved up in a
#'   recursive way.
#' @param pd_nested A nested parse table to reorganize.
#' @param token Character vector with tokens that can cause a child to be moved
#'   up.
#' @details A child is moved up under the following conditions:
#'
#'   * The parent of the child to be considered for moving up (that is simply
#'     the corresponding row in `pd_nested`) is not a top-level expression.
#'     Otherwise, the second expression may be indented due to operator
#'     indention(e.g. in "1+1; 1+1").
#'   * The child has one of `token` in the parse table. The first token is not
#'     considered to propper handle signs (see [pd_has_token_not_first()]).
#'   * One of `token` also appears before or after the child in `pd_nested`.
#'     For example, there is no need to move any child up in the expression
#'     "{1+1}", since then, operator and bracket indention would conflict.
#' @importFrom purrr map_lgl
move_children_up <- function(pd_nested,
                             token = c("'+'", "'-'", "SPECIAL", "'/'", "'*'")) {

  move_up <- pd_nested$parent > 0 &
    map_lgl(pd_nested$child, pd_has_token_not_first, token) &
    token_is_adjacent(pd_nested, token)

  out <- bind_rows(pd_nested, pd_nested$child[move_up]) %>%
    mutate(old_parent = id %in% parent) %>%
    filter(!old_parent) %>%
    select(-old_parent) %>%
    arrange(line1, col1)
  if (any(move_up)) {
    out <- re_nest(out)
  } else {
    out
  }
}

#' Are they neighbours?
#'
#' Check whether a token is positioned next to another token in a parse table.
#' @param pd A parse table.
#' @param token A vector of tokens to check.
token_is_adjacent <- function(pd, token) {
  lead <- lead(pd$token %in% token, default = FALSE)
  lag  <-  lag(pd$token %in% token, default = FALSE)
  lead | lag
}
