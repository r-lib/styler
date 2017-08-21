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
  eq_sub <- which(pd$token == "EQ_SUB")
  if (length(eq_sub) == 0) return(pd)
  has_line_break <- which(pd$lag_newlines > 0)
  indent_indices <- intersect(eq_sub + 1, has_line_break)
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
  potential_triggers <- which(pd$token %in% token)
  needs_indention <- needs_indention(pd, potential_triggers)
  trigger <- potential_triggers[needs_indention][1]
  if (is.na(trigger)) return(numeric(0))
  start <- trigger + 1
  stop <- npd - ifelse(indent_last, 0, 1)
  which(between(seq_len(npd), start, stop))
}


#' Check whether indention is needed
#'
#' Checks for each potential trigger token in `pd` whether it actually should
#' cause indention.
#' @param potential_triggers A vector with indices of the potential trigger
#'   tokens in `pd`.
#' @inheritParams needs_indention_one
needs_indention <- function(pd, potential_triggers) {
  map_lgl(potential_triggers, needs_indention_one, pd = pd)
}


#' Check whether indention is needed
#'
#' Indention is needed if and only if there is no multi-line token between the
#' trigger and the first line break.
#' @param pd A parse table.
#' @param potential_trigger the index of the token in the parse table
#'   for which it should be checked whether it should trigger indention.
#' @return Returns `TRUE` if indention is needed, `FALSE` otherwise.
#' @return `TRUE` if indention is needed, `FALSE` otherwise.
needs_indention_one <- function(pd, potential_trigger) {
  before_first_break <- which(pd$lag_newlines > 0)[1] - 1
  if (is.na(before_first_break)) return(FALSE)
  !any(pd$multi_line[potential_trigger:before_first_break])
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
#' lose information for the last token. `spaces` is left as is in
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
