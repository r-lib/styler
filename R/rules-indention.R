#' @describeIn update_indention Inserts indention based on round, square and
#'   curly brackets.
#' @keywords internal
indent_braces <- function(pd, indent_by) {
  indent_indices <- compute_indent_indices(
    pd,
    token_opening = c("'('", "'['", "'{'"),
    token_closing = c("')'", "']'", "'}'")
  )
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  set_unindention_child(pd, token = "')'", unindent_by = indent_by)
}

#' Revert the indention of function declaration header
#'
#' Necessary for consistent indention of the function declaration header.
#' @param pd A parse table.
#' @seealso set_unindention_child update_indention_ref_fun_dec
#' @keywords internal
unindent_fun_dec <- function(pd) {
  if (is_function_dec(pd)) {
    idx_closing_brace <- which(pd$token %in% "')'")
    fun_dec_head <- seq2(2L, idx_closing_brace)
    pd$indent[fun_dec_head] <- 0L
  }
  pd
}

#' @describeIn update_indention Indents *all* tokens after `token` - including
#'   the last token.
#' @keywords internal
indent_op <- function(pd,
                      indent_by,
                      token = c(
                        math_token,
                        logical_token,
                        special_token,
                        "LEFT_ASSIGN",
                        "EQ_ASSIGN",
                        "'$'"
                      )) {
  indent_indices <- compute_indent_indices(pd, token)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Updates indention for token EQ_SUB. Only differs
#'   from [indent_op()] in the sense that not all subsequent tokens in the parse
#'   table are necessarily indented, as `EQ_SUB` and `EQ_FORMALS` can occur
#'   multiple times in a parse table.
#'   occurs is not indented (see[compute_indent_indices()])
#' @keywords internal
indent_eq_sub <- function(pd,
                          indent_by,
                          token = c("EQ_SUB", "EQ_FORMALS")) {
  eq_sub <- which(pd$token %in% token)
  if (length(eq_sub) == 0) {
    return(pd)
  }
  has_line_break <- which(pd$lag_newlines > 0)
  indent_indices <- intersect(eq_sub + 1, has_line_break)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Is used to indent for / while / if / if-else
#'   statements that do not have curly parenthesis.
#' @keywords internal
indent_without_paren <- function(pd, indent_by = 2) {
  pd %>%
    indent_without_paren_for_while_fun(indent_by) %>%
    indent_without_paren_if_else(indent_by)
}

#' Update the indention reference
#'
#' @param pd_nested A nested parse table.
#' @name update_indention_ref
#' @keywords internal
NULL

#' @describeIn update_indention_ref Updates the reference pos_id for all
#'   tokens in `pd_nested` if `pd_nested` contains a function declaration.
#'   Tokens inside a function declaration are are re-indented,
#'   that is, they are indented up to the level at which the token FUNCTION
#'   ends in terms of col2.
#' @examples
#' \dontrun{
#' a <- function(x,
#'               y) {
#'   x + y
#' }
#' }
#' @importFrom rlang seq2
#' @keywords internal
update_indention_ref_fun_dec <- function(pd_nested) {
  if (pd_nested$token[1] == "FUNCTION") {
    seq <- seq2(3, nrow(pd_nested) - 2)
    pd_nested$indention_ref_pos_id[seq] <- pd_nested$pos_id[2]
  }
  pd_nested
}
