#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [initialize_attributes()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @param token The token the indention should be based on.
#' @name update_indention
NULL

#' @describeIn update_indention Inserts indention based on round, square and
#'   curly brackets.
indent_braces <- function(pd, indent_by) {
  indent_indices <- compute_indent_indices(
    pd,
    token_opening = c("'('", "'['", "'{'"),
    token_closing = c("')'", "']'", "'}'")
  )
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  set_unindention_child(pd, token = "')'", unindent_by = indent_by)
}

#' @describeIn update_indention Indents operators
indent_op <- function(pd,
                      indent_by,
                      token = c(
                        math_token,
                        logical_token,
                        special_token,
                        "LEFT_ASSIGN",
                        "'$'")
                      ) {
  indent_indices <- compute_indent_indices(pd, token)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Updates indention for token EQ_SUB. Only differs
#'   from indent_op in the sense that the last token on the table where EQ_SUB
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
  indent_indices <- compute_indent_indices(pd, token)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Is used to indent for / while / if / if-else
#'   statements that do not have curly parenthesis.
indent_without_paren <- function(pd, indent_by = 2) {
  pd %>%
  indent_without_paren_for_while(indent_by) %>%
  indent_without_paren_if_else(indent_by)
}

#' @describeIn update_indention Is used to indent for and while statements.
indent_without_paren_for_while <- function(pd, indent_by) {
  nrow <- nrow(pd)
  if (!(pd$token[1] %in% c("FOR", "WHILE", "FUNCTION"))) return(pd)
  if (is_curly_expr(pd$child[[nrow]])) return(pd)
  pd$indent[nrow] <- indent_by
  pd
}

#' @describeIn update_indention Is used to indent if and if-else statements.
#' @importFrom rlang seq2
indent_without_paren_if_else <- function(pd, indent_by) {
  has_if_without_curly <-
    pd$token[1] %in% c("IF", "WHILE") && pd$child[[5]]$token[1] != "'{'"
  if (has_if_without_curly) {
    pd$indent[5] <- indent_by
  }

  has_else_without_curly_or_else_chid <-
    any(pd$token == "ELSE") &&
    pd$child[[7]]$token[1] != "'{'" &&
    pd$child[[7]]$token[1] != "IF"
  if (has_else_without_curly_or_else_chid) {
    pd$indent[seq(7, nrow(pd))] <- indent_by
  }
  pd
}

#' Compute the indices that need indention
#'
#' Based on `token`, find the rows in `pd` that need to be indented.
#' @param pd A parse table.
#' @param token_opening A character vector with tokens that could induce
#'   indention for subsequent tokens.
#' @param token_closing A character vector with tokens that could terminate
#'   indention for previous tokens. If `NULL` (the default), indention should
#'   end with the last token in the parse table.
#' @details
#' Two cases are fundamentally different:
#'
#' * Indention based on operators (e.g '+'), where all subsequent tokens should
#'   be indented.
#' * Indention based on braces (e.g. '('), where just the tokens between the
#'   opening and the closing brace have to be indented.
#'
#' To cover the second case, we need `token_closing` because it cannot be taken
#' for granted that `token_closing` is always the last token in `pd`. For
#' example in if-else expressions, this is not the case and indenting
#' everything between '(' and the penultimate token would result in the wrong
#' formatting.
#' @importFrom rlang seq2
compute_indent_indices <- function(pd,
                                   token_opening,
                                   token_closing = NULL) {
  npd <- nrow(pd)
  potential_triggers <- which(pd$token %in% token_opening)
  needs_indention <- needs_indention(pd, potential_triggers)
  trigger <- potential_triggers[needs_indention][1]
  if (is.na(trigger)) return(numeric(0))
  start <- trigger + 1
  stop <- ifelse(is.null(token_closing),
                 npd,
                 which(pd$token %in% token_closing) - 1)

  seq2(start, stop)
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
#' @importFrom rlang seq2
needs_indention_one <- function(pd, potential_trigger) {
  before_first_break <- which(pd$lag_newlines > 0)[1] - 1
  if (is.na(before_first_break)) return(FALSE)
  !any(pd$multi_line[seq2(potential_trigger, before_first_break)])
}



#' Set the multi-line column
#'
#' Sets the column `multi_line` in `pd` by checking row-wise whether any child
#'   of a token is a multi-line token.
#' @param pd A parse table.
#' @importFrom purrr map_lgl
set_multi_line <- function(pd) {
  pd$multi_line <- map_lgl(pd$child, pd_is_multi_line)
  pd
}

#' Check whether a parse table is a multi-line token
#'
#' A token is a multi-line expression if and only if:
#'
#' * it contains a line break.
#' * it has at least one child that is a multi-line expression itself.
#' @param pd A parse table.
pd_is_multi_line <- function(pd) {
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
#' @return A parse table with synchronized `lag_newlines` and `newlines` columns.
#' @seealso choose_indention
update_newlines <- function(pd) {
  npd <- nrow(pd) - 1
  pd$newlines[seq_len(npd)] <- pd$lag_newlines[seq_len(npd) + 1]
  pd
}
