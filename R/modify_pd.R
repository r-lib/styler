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
  indent_indices <- compute_indent_indices(pd, token = "'('")
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  set_unindention_child(pd, token = "')'", unindent_by = indent_by)
}

#' @rdname update_indention
indent_curly <- function(pd, indent_by) {
  indent_indices <- compute_indent_indices(pd, token = "'{'")
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  set_unindention_child(pd, token = "'}'", unindent_by = indent_by)
}

#' @describeIn update_indention Indents operatos
indent_op <- function(pd,
                      indent_by,
                      token = c(math_token,
                                logical_token,
                                special_token,
                                "LEFT_ASSIGN", 
                                "'$'")) {
  indent_indices <- compute_indent_indices(pd, token, indent_last = TRUE)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Upates indention for token EQ_SUB. Only differs
#'   from ident_op in the sense that the last token on the talbe where EQ_SUB
#'   occurs is not indented (see[compute_indent_indices()])
indent_eq_sub <- function(pd,
                          indent_by,
                          token = "EQ_SUB") {
  indent_indices <- compute_indent_indices(pd, token, indent_last = FALSE)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}


#' @describeIn update_indention Same as indent_op, but only indents one token
#'   after `token`, not all remaining.
indent_assign <- function(pd, indent_by, token = NULL) {
  indent_indices <- compute_indent_indices(pd, token, indent_last = TRUE)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Is used to indent if / while / for statements
#'   that do not have curly brackets.
indent_without_paren <- function(pd, indent_by = 2) {
  nrow <- nrow(pd)
  if (!(pd$token[1] %in% c("IF", "FOR", "WHILE"))) return(pd)
  if (pd$lag_newlines[nrow] == 0) return(pd)
  pd$indent[nrow] <- indent_by
  pd
}

#' Compute the indices that need indention
#'
#' Based on `token`, find the rows in `pd` that need to be indented.
#' @param pd A parse table.
#' @param token A character vector with tokens.
#' @param indent_last Flag to indicate whether the last token in `pd` should
#'   be indented or not. See 'Details'.
#' @details
#'  For example when `token` is a parenthesis, the closing parenthesis does not
#'  need indention, but if token is something else, for example a plus (+), the
#'  last token in `pd` needs indention.
compute_indent_indices <- function(pd, token = "'('", indent_last = FALSE) {
  npd <- nrow(pd)
  opening <- which(pd$token %in% token)[1]
  if (!needs_indention(pd, opening)) return(numeric(0))
  start <- opening + 1
  stop <- npd - ifelse(indent_last, 0, 1)
  which(between(seq_len(npd), start, stop))
}


#' Check whether indention is needed
#'
#' @param pd A parse table.
#' @param opening the index of the opening parse table. Since always computed
#'   before this function is called, it is included as an argument so it does
#'   not have to be recomputed.
#' @return returns `TRUE` if indention is needed, `FALSE` otherwise. Indention
#'   is needed if and only if:
#'     * the opening token is not `NA`.
#'     * if there is a multi-line token before the first line break.
#' @return `TRUE` if indention is needed, `FALSE` otherwise.
needs_indention <- function(pd, opening) {
  if (is.na(opening)) return(FALSE)
  before_first_break <- which(pd$lag_newlines > 0)[1] - 1
  if (is.na(before_first_break)) return(FALSE)
  !any(pd$multi_line[opening:before_first_break])
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

#' Update the newlines attribute
#'
#' As we work only with the `lag_newlines` attribute for setting the linebreaks,
#' (R/rules-line_break.R) but we need `newlines` to determine
#' whether or not to set `spaces` (R/rules-spacing.R), we have to update the
#' attribute. We cannot simply use `dplyr::lead(pd$lag_newlines)` since we would
#' loose information for the last token. `spaces` is left as is in
#' R/rules-spacing.R for tokens at the end of a line since this allows styling
#' without touching indention.
#' @param pd A parse table.
#' @return A parse table with syncronized `lag_newlines` and `newlines` columns.
#' @seealso choose_indention
update_newlines <- function(pd) {
  npd <- nrow(pd) - 1
  pd$newlines[seq_len(npd)] <- pd$lag_newlines[seq_len(npd) + 1]
  pd
}
