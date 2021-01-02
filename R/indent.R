#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [default_style_guide_attributes()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @param token The token the indention should be based on.
#' @name update_indention
#' @keywords internal
NULL

#' @describeIn update_indention Is used to indent for and statements and function
#'   definitions without parenthesis.
#' @keywords internal
indent_without_paren_for_while_fun <- function(pd, indent_by) {
  tokens <- c("FOR", "WHILE", "FUNCTION")
  nrow <- nrow(pd)
  if (!(pd$token[1] %in% tokens)) {
    return(pd)
  }
  if (is_curly_expr(pd$child[[nrow]])) {
    return(pd)
  }

  if (pd$newlines[length(pd$newlines) - 1] == 0) {
    return(pd)
  }
  pd$indent[nrow] <- indent_by
  pd
}

#' @describeIn update_indention Is used to indent if and if-else statements.
#' @importFrom rlang seq2
#' @keywords internal
indent_without_paren_if_else <- function(pd, indent_by) {
  expr_after_if <- next_non_comment(pd, which(pd$token == "')'")[1])
  is_if <- pd$token[1] %in% "IF"
  has_if_without_curly <-
    is_if && pd$child[[expr_after_if]]$token[1] != "'{'"
  if (!is_if) {
    return(pd)
  }
  needs_indention_now <- pd$lag_newlines[next_non_comment(pd, which(pd$token == "')'"))] > 0

  if (needs_indention_now) {
    pd$indent[expr_after_if] <- indent_by
  }



  else_idx <- which(pd$token == "ELSE")
  if (length(else_idx) == 0) {
    return(pd)
  }
  expr_after_else_idx <- next_non_comment(pd, else_idx)
  has_else_without_curly_or_else_chid <-
    any(pd$token == "ELSE") &&
      pd$child[[expr_after_else_idx]]$token[1] != "'{'" &&
      pd$child[[expr_after_else_idx]]$token[1] != "IF"

  needs_indention_now <- pd$lag_newlines[next_non_comment(pd, which(pd$token == "ELSE"))] > 0

  if (has_else_without_curly_or_else_chid && needs_indention_now) {
    pd$indent[seq(else_idx + 1, nrow(pd))] <- indent_by
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
#' @keywords internal
compute_indent_indices <- function(pd,
                                   token_opening,
                                   token_closing = NULL) {
  npd <- nrow(pd)
  potential_triggers <- which(pd$token %in% token_opening)
  needs_indention <- needs_indention(pd, potential_triggers,
    other_trigger_tokens = c("EQ_SUB", "EQ_FORMALS")
  )
  trigger <- potential_triggers[needs_indention][1]
  if (is.na(trigger)) {
    return(numeric(0))
  }
  start <- trigger + 1
  if (is.null(token_closing)) {
    stop <- npd
  } else {
    stop <- last(which(pd$token %in% token_closing)[needs_indention]) - 1
  }

  seq2(start, stop)
}


#' Check whether indention is needed
#'
#' Checks for each potential trigger token in `pd` whether it actually should
#' cause indention.
#' @param potential_triggers_pos A vector with indices of the potential trigger
#'   tokens in `pd`.
#' @inheritParams needs_indention_one
#' @keywords internal
needs_indention <- function(pd,
                            potential_triggers_pos,
                            other_trigger_tokens = NULL) {
  map_lgl(potential_triggers_pos, needs_indention_one,
    pd = pd, other_trigger_tokens = other_trigger_tokens
  )
}


#' Check whether indention is needed
#'
#' Determine whether the tokens corresponding to `potential_trigger_pos` should
#' cause indention, considering that there might be other potential triggers
#' `other_trigger_tokens` that are going to cause indention.
#' Indention is needed if the two conditions apply:
#'
#' * there is no multi-line token between the trigger and the first line break.
#' * there is no other token between the potential trigger and the first line
#'   break that is going to cause indention. Note that such an other trigger
#'   only causes indention if there is a line break after that other triggering
#'   token, not otherwise. If it causes indention, it is said to be an active
#'   trigger, if it does not, it is called an inactive trigger.
#'   See 'Details' for an example where there is an other trigger token, but
#'   since the next token is on the same line as the other trigger,
#'   the trigger is passive.
#' @param pd A parse table.
#' @param potential_trigger_pos the index of the token in the parse table
#'   for which it should be checked whether it should trigger indention.
#' @return Returns `TRUE` if indention is needed, `FALSE` otherwise.
#' @param other_trigger_tokens Other tokens that are going to cause indention
#'   if on the same line as the token corresponding to `potential_trigger` and
#'   directly followed by a line break.
#' @return `TRUE` if indention is needed, `FALSE` otherwise.
#' @importFrom rlang seq2
#' @keywords internal
#' @examples
#' style_text(c(
#'   "call(named = c,",
#'   "named = b)"
#' ), strict = FALSE)
needs_indention_one <- function(pd,
                                potential_trigger_pos,
                                other_trigger_tokens) {
  before_first_break <- which(pd$lag_newlines > 0)[1] - 1
  if (is.na(before_first_break)) {
    return(FALSE)
  }
  row_idx_between_trigger_and_line_break <- seq2(
    potential_trigger_pos, before_first_break
  )
  multi_line_token <- pd_is_multi_line(
    pd[row_idx_between_trigger_and_line_break, ]
  )
  remaining_row_idx_between_trigger_and_line_break <- setdiff(
    row_idx_between_trigger_and_line_break,
    potential_trigger_pos
  )

  other_trigger_on_same_line <- (
    pd$token[remaining_row_idx_between_trigger_and_line_break] %in%
      other_trigger_tokens
  )
  line_break_after_other_trigger <-
    pd$lag_newlines[remaining_row_idx_between_trigger_and_line_break + 1L] > 0L

  active_trigger_on_same_line <-
    other_trigger_on_same_line & line_break_after_other_trigger

  !any(multi_line_token) & !any(active_trigger_on_same_line)
}



#' Set the multi-line column
#'
#' Sets the column `multi_line` in `pd` by checking row-wise whether any child
#' of a token is a multi-line token.
#' @param pd A parse table.
#' @importFrom purrr map_lgl
#' @keywords internal
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
#' @keywords internal
pd_is_multi_line <- function(pd) {
  any(pd$multi_line, pd$lag_newlines > 0)
}

#' Update the newlines attribute
#'
#' As we work only with the `lag_newlines` attribute for setting the line
#' breaks (`R/rules-line_breaks.R`), but we need `newlines` to determine
#' whether or not to set `spaces` (`R/rules-spaces.R`), we have to update the
#' attribute. We cannot simply use `dplyr::lead(pd$lag_newlines)` since we would
#' lose information for the last token. `spaces` is left as is in
#' R/rules-spacing.R for tokens at the end of a line since this allows styling
#' without touching indention.
#' @param pd A parse table.
#' @return A parse table with synchronized `lag_newlines` and `newlines` columns.
#' @seealso choose_indention
#' @keywords internal
update_newlines <- function(pd) {
  seq_pd <- seq_len(nrow(pd) - 1)
  pd$newlines[seq_pd] <- pd$lag_newlines[seq_pd + 1]
  pd
}
