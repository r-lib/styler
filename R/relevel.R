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
  pd_nested %>%
    post_visit(c(flatten_operators_one))
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
  pd_token_left <- c(special_token, math_token, "'$'")
  pd_token_right <- c(
    special_token, "LEFT_ASSIGN", if (parser_version_get() > 1) "EQ_ASSIGN",
    "'+'", "'-'"
  )
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
#' @keywords internal
flatten_pd <- function(pd_nested, token, child_token = token, left = TRUE) {
  token_pos_candidates <- which(pd_nested$token[-1] %in% token) + 1
  if (length(token_pos_candidates) == 0) {
    return(pd_nested)
  }
  token_pos <- token_pos_candidates[if_else(left, 1, length(token_pos_candidates))]
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
    arrange(pos_id)
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
    terminal = FALSE
  )
}


#   ____________________________________________________________________________
#   Relocate EQ_ASSIGN                                                      ####

#' Relocate the expressions containing the token `EQ_ASSIGN` within the nested
#' parse table
#'
#' Although syntactically identical, [utils::getParseData()] does not produce
#' the same hierarchy of the parse table (parent and id relationship) for `<-`
#' and `=` (See 'Examples').
#' This is considered to be a bug and causes problems because the
#' nested parse table constructed with [compute_parse_data_nested()] is not
#' consistent if `EQ_ASSIGN` occurs in the expression to style. In particular,
#' `EQ_ASSIGN` and the tokens to its left and right are located too high up in
#' the hierarchy of the nested parse data. Hence, this function wraps the
#' sub-expression into an expression, similar to [wrap_expr_in_curly()].
#' Since `wrap_expr_in_curly()` is called from within a visitor
#' (and `relocate_eq_assign()` not), we need to
#' wrap the the implementation [relocate_eq_assign_nest()] that operates on
#' *nests* into a visitor call.
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
  if (parser_version_get() < 2) {
    pd %>%
      post_visit(c(relocate_eq_assign_nest))
  } else {
    pd
  }
}


#' Relocate all assignment expressions that contain `EQ_ASSIGN` within a *nest*
#'
#' Implements the relocation of an `EQ_ASSIGN` and associated tokens
#' within a *nest* (nested parse table at one level of nesting).
#' Note that one assignment expression (such as "a = b = c") can include
#' multiple assignment operators, an assignment involves just one assignment
#' operator.
#' For the relocation of assignment expressions that contain `EQ_ASSIGN` within
#' a *nest*, we need to first find the expressions that contain `=` and then
#' split the *nest* into parse tables each containing one such assignment
#' expression and then relocate each of them separately.
#' We can't do all of them together because:
#'
#'  * An assignment can contain more than just three tokens, e.g. (a <- b <- c).
#'  * Two assignments can be in the same nest although they don't belong to the
#'    same assignment (if-else statement).
#'
#' Please refer to the section 'Examples' in [relocate_eq_assign()] for details.
#' @param pd A parse table.
#' @importFrom rlang seq2
#' @keywords internal
relocate_eq_assign_nest <- function(pd) {
  idx_eq_assign <- which(pd$token == "EQ_ASSIGN")
  if (length(idx_eq_assign) > 0) {
    block_id <- find_block_id(pd)
    blocks <- split(pd, block_id)
    pd <- map_dfr(blocks, relocate_eq_assign_one)
  }
  pd
}

#' Find the block to which a token belongs
#'
#' Two assignment tokens `EQ_ASSIGN` belong to the same block if they are not
#' separated by more than one token. Token between `EQ_ASSIGN` tokens belong
#' to the `EQ_ASSIGN` token occurring before them, except the token right before
#' `EQ_ASSING` already belongs to the `EQ_ASSING` after it.
#' @param pd A parse table.
#' @keywords internal
find_block_id <- function(pd) {
  idx_eq_assign <- which(pd$token == "EQ_ASSIGN")
  eq_belongs_to_block <- c(0, diff(idx_eq_assign) > 2)

  empty_seq <- rep(0, nrow(pd))
  empty_seq[idx_eq_assign - 1] <- eq_belongs_to_block
  block_id <- cumsum(empty_seq)
  block_id
}

#' Relocate an assignment expression
#'
#' Relocates an assignment expression within a parse table containing one
#' assignment expression. Note that one assignment can include multiple
#' assignment operators such as "a = b = c".
#' @param pd A parse table with one assignment expression to relocate.
#' @keywords internal
relocate_eq_assign_one <- function(pd) {
  idx_eq_assign <- which(pd$token == "EQ_ASSIGN")
  eq_ind <- seq2(idx_eq_assign[1] - 1L, last(idx_eq_assign) + 1L)
  eq_expr <- pd[eq_ind, ] %>%
    wrap_expr_in_expr() %>%
    add_line_col_to_wrapped_expr() %>%
    remove_attributes(c(
      "multi_line", "indention_ref_pos_id",
      "newlines", "indent", "spaces", "lag_newlines"
    ))
  eq_expr$id <- NA
  eq_expr$parent <- NA
  non_eq_expr <- pd[-eq_ind, ]
  pd <- bind_rows(eq_expr, non_eq_expr) %>%
    arrange(pos_id)
  pd
}

#' Adds line and col information to an expression from its child
#'
#' @param pd A parse table.
#' @importFrom rlang abort
#' @keywords internal
add_line_col_to_wrapped_expr <- function(pd) {
  if (nrow(pd) > 1) abort("pd must be a wrapped expression that has one row.")
  pd$line1 <- pd$child[[1]]$line1[1]
  pd$line2 <- last(pd$child[[1]]$line2)
  pd$col1 <- pd$child[[1]]$col1[1]
  pd$col2 <- last(pd$child[[1]]$col2)
  pd
}
