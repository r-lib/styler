#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [create_filler()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @param token The token the indention should be based on.
#' @name update_indention
NULL

#' @describeIn update_indention Inserts indetion based on round brackets.
indent_round <- function(pd, indent_by) {
  opening <- which(pd$token == "'('")
  indention_needed <- needs_indention(pd, token = "'('", opening[1])
  if (indention_needed) {
    start <- opening + 1
    stop <- nrow(pd) - 1
    if (start > stop) return(pd)
    pd$indent <- pd$indent +
      ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)
  }

  pd %>%
    set_unindention_child(token = "')'", unindent_by = indent_by)
}
#' @rdname update_indention
indent_curly <- function(pd, indent_by) {
  opening <- which(pd$token == "'{'")
  indention_needed <- needs_indention(pd, token = "'{'", opening[1])
  if (indention_needed) {
    start <- opening + 1
    stop <- nrow(pd) - 1
    if (start > stop) return(pd)
    pd$indent <- pd$indent +
      ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)
  }
  pd %>%
    set_unindention_child(token = "'}'", unindent_by = indent_by)
}

#' Check whether indention is needed
#'
#' @param pd A parse table.
#' @param token Which token the check should be based on.
#' @param opening the index of the opening parse table. Since always computed
#'   before this function is called, it is included as an argument so it does
#'   not have to be recomputed.
#' @return returns `TRUE` if indention is needed, `FALSE` otherwise. Indention
#'   is needed:
#'     * if `token` occurs in `pd`.
#'     * if there is no child that starts on the same line as `token` and
#'       "opens" indention without closing it on this line.
#' @return `TRUE` if indention is needed, `FALSE` otherwise.
needs_indention <- function(pd, token = "'('", opening) {
  if (is.na(opening)) return(FALSE)
  before_first_break <- which(pd$lag_newlines > 0)[1] - 1
  if (is.na(before_first_break)) return(FALSE)
  !any(pd$multi_line[opening:before_first_break])
}

#' @rdname update_indention
indent_op <- function(pd, indent_by, token = c(math_token,
                                               "SPECIAL-PIPE")) {
  opening <- which(pd$token %in% token)
  if (needs_indention(pd, token, opening[1])) {
    start <- opening[1] + 1
    stop <- nrow(pd)
    pd$indent <- pd$indent +
      ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)
  }
  pd
}

#' @describeIn update_indention Same as indent_op, but only indents one token
#'   after `token`, not all remaining.
indent_assign <- function(pd, indent_by, token = c("LEFT_ASSIGN", "
                                                   EQ_ASSIGN")) {
  opening <- which(pd$token %in% token)
  if (needs_indention(pd, token, opening[1])) {
    start <- opening + 1
    stop <- start + 1
    pd$indent <- pd$indent +
      ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)
  }
  pd
}

#' @describeIn update_indention Is used to indent if / while / for statements
#'   that do not have curly brackets.
indent_without_paren <- function(pd, indent_by = 2) {
  nrow <- nrow(pd)
  if (!(pd$token[1] %in% c("IF", "FOR", "WHILE"))) return(pd)
  if (pd$child[[nrow]]$token[1] == "'{'") return(pd)
  pd$indent[nrow] <- indent_by
  pd
}

#' Set the multi-line column
#'
#' Sets the column `multi_line` in `pd` by checking row-wise whether any child
#'   of a token is a multi-line token.
#' @param pd A parse table.
#' @importFrom purrr map_lgl
set_multi_line <- function(pd) {
  pd$multi_line <- map_lgl(pd$child, token_is_multi_line)
  pd
}

#' Check whether a parse table is a multi-line token
#'
#' A token is a multi-line expression if and only if:
#'
#' * it contains a line break.
#' * it has at least one child that is a multi-line expression itself.
#' @param pd A parse table.
token_is_multi_line <- function(pd) {
  any(pd$multi_line, pd$lag_newlines > 0)
}


#' Strip EOL spaces
#'
#' Remove end-of-line spaces.
#' @param pd_flat A flat parse table.
#' @return A nested parse table.
strip_eol_spaces <- function(pd_flat) {
  pd_flat$spaces <- pd_flat$spaces * (lead(pd_flat$lag_newlines, default = 0) == 0)
  pd_flat
}
