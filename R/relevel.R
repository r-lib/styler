#   ____________________________________________________________________________
#   Flatten the parse table                                                 ####

#' Flatten some token in the nested parse table based on operators
#'
#' Certain tokens are not placed optimally in the nested parse data with
#' [compute_parse_data_nested()]. For example, the token of arithmetic
#' operations 1 + 1 + 1 should all be on the same level of nesting since the
#' indention is the same for all but the first two terminals. Setting the
#' indention correctly is easier to achieve if they are put on the same level of
#' nesting.
#' @param pd_nested A nested parse table to partially flatten.
#' @keywords internal
flatten_operators <- function(pd_nested) {
  post_visit_one(pd_nested, flatten_operators_one)
}

#' Flatten one level of nesting with its child
#'
#' Flattening is done in two ways. We can flatten a parse table by moving the
#' left hand token of an operator one level up. Or doing that with the right
#' hand token.
#' @param pd_nested A nested parse table.
#' @include token-define.R
#' @keywords internal
flatten_operators_one <- function(pd_nested) {
  pd_token_left <- c(special_token, "PIPE", math_token, "'$'")
  pd_token_right <- c(
    special_token, "PIPE", "LEFT_ASSIGN",
    "EQ_ASSIGN",
    "'+'", "'-'", "'~'"
  )
  pd_nested %>%
    flatten_pd(pd_token_left, left = TRUE) %>%
    flatten_pd(pd_token_right, left = FALSE)
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
#' @keywords internal
flatten_pd <- function(pd_nested, token, child_token = token, left = TRUE) {
  token_pos_candidates <- which(pd_nested$token[-1] %in% token) + 1
  if (length(token_pos_candidates) == 0) {
    return(pd_nested)
  }
  token_pos <- token_pos_candidates[
    ifelse(left, 1, length(token_pos_candidates))
  ]
  if (left) {
    pos <- previous_non_comment(pd_nested, token_pos)
  } else {
    pos <- next_non_comment(pd_nested, token_pos)
  }
  if (pos < 1) {
    return(pd_nested)
  }
  if (!any(pd_nested$child[[pos]]$token[-1] %in% child_token)) {
    return(pd_nested)
  }
  bind_with_child(pd_nested, pos)
}

#' Bind a parse table with one of its children
#'
#' Bind a parse table with one of its children and return parse table, ordered
#' according to the appearance of the tokens.
#' @param pd_nested A nested parse table.
#' @param pos The position of the child to bind.
#' @keywords internal
bind_with_child <- function(pd_nested, pos) {
  pd_nested %>%
    slice(-pos) %>%
    bind_rows(pd_nested$child[[pos]]) %>%
    arrange_pos_id()
}

#' Wrap an expression into an expression
#'
#' Takes a parse table and wraps it in a new parse table that contains the
#' expression as a child.
#' @param pd A parse table.
#' @keywords internal
wrap_expr_in_expr <- function(pd) {
  create_tokens(
    "expr", "",
    pos_ids = create_pos_ids(pd, 1, after = FALSE),
    child = pd,
    terminal = FALSE,
    stylerignore = pd$stylerignore[1],
    indents = pd$indent[1]
  )
}


#   ____________________________________________________________________________
#   Relocate EQ_ASSIGN                                                      ####

#' Relocate the expressions containing the token `EQ_ASSIGN` within the nested
#' parse table
#'
#' This used to be relevant when `{styler}` supported R versions `< 3.5`, and,
#' therefore, parser version `1`, which had a bug that did not produce the same
#' hierarchy of the parse table (parent and id relationship) for `<-` and `=`
#' (See 'Examples').
#'
#' @param pd A parse table.
#' @examples
#' styler:::get_parse_data("a <- b <- 3")
#' styler:::get_parse_data("a  = b = 3")
#' styler:::get_parse_data(
#'   "x = 5
#'   if(x >= 5)
#'   y = TRUE else
#'   y = FALSE",
#' )
#' styler:::get_parse_data(
#'   "x <- 5
#'   if(x >= 5)
#'   y <- TRUE else
#'   y <- FALSE",
#' )
#' @keywords internal
relocate_eq_assign <- function(pd) {
  pd
}
