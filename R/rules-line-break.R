# A { should never go on its own line
remove_line_break_before_curly_opening <- function(pd) {
  rm_break <- (pd$token_after == "'{'") & (pd$token != "COMMENT")
  pd$lag_newlines[lag(rm_break)] <- 0L
  pd
}

set_line_break_around_comma <- function(pd) {
  comma_with_line_break_that_can_be_removed_before <-
    (pd$token == "','") &
    (pd$lag_newlines > 0) &
    (pd$token_before != "COMMENT") &
    (lag(pd$token) != "'['")
  pd$lag_newlines[comma_with_line_break_that_can_be_removed_before] <- 0L
  pd$lag_newlines[lead(comma_with_line_break_that_can_be_removed_before)] <- 1L
  pd
}

style_line_break_around_curly <- function(strict, pd) {
  if (is_curly_expr(pd) && nrow(pd) > 2) {
    closing_before <- pd$token == "'}'"
    opening_before <- (pd$token == "'{'") & (pd$token_after != "COMMENT")
    to_break <- lag(opening_before, default = FALSE) | closing_before
    len_to_break <- sum(to_break)
    pd$lag_newlines[to_break] <- ifelse(rep(strict, len_to_break),
      1L,
      pmax(1L, pd$lag_newlines[to_break])
    )
  }
  pd
}

# if ) follows on }, don't break line
remove_line_break_before_round_closing_after_curly <- function(pd) {
  round_after_curly <- pd$token == "')'" & (pd$token_before == "'}'")
  pd$lag_newlines[round_after_curly] <- 0L
  pd
}

remove_line_break_before_round_closing_fun_dec <- function(pd) {
  if (is_function_dec(pd)) {
    round_after <- pd$token == "')'" & pd$token_before != "COMMENT"
    pd$lag_newlines[round_after] <- 0L
  }
  pd
}


#' @importFrom rlang seq2
add_line_break_after_pipe <- function(pd) {
  is_special <- pd$token == c("SPECIAL-PIPE") & pd$token_after != "COMMENT"
  if (any(pd$lag_newlines != 0L)) {
    pd$lag_newlines[lag(is_special)] <- 1L
  }
  pd
}


#' Set line break for multi-line function calls
#' @param pd A parse table.
#' @param except_token_after A character vector with tokens after "'('" that do
#'   not cause a line break after "'('".
#' @param except_text_before A character vector with text before "'('" that do
#'   not cause a line break after "'('".
#' @param except_token_before A character vector with text before "')'" that do
#'   not cause a line break before "')'".
#' @name set_line_break_if_call_is_multi_line
#' @importFrom rlang seq2
#' @keywords internal
NULL

#' @describeIn set_line_break_if_call_is_multi_line Sets line break after
#'   opening parenthesis.
#' @keywords internal
set_line_break_after_opening_if_call_is_multi_line <-
  function(pd,
             except_token_after = NULL,
             except_text_before = NULL) {
    if (!is_function_call(pd) && !is_subset_expr(pd)) return(pd)
    npd <- nrow(pd)
    seq_x <- seq2(3L, npd - 1L)
    is_multi_line <- any(
      (pd$lag_newlines[seq_x] > 0) |
        (pd$token[seq_x] == "COMMENT")
    )
    if (!is_multi_line) {
      return(pd)
    }
    break_pos <- find_line_break_position_in_multiline_call(pd)

    exception_pos <- c(
      which(pd$token %in% except_token_after),
      if_else(pd$child[[1]]$text[1] %in% except_text_before, break_pos, NA)
    )
    pd$lag_newlines[setdiff(break_pos, exception_pos)] <- 1L
    pd
  }


#' Find index of the token before which the line should be broken
#'
#' Given a multi-line function call parse table, this function finds the
#' position of the first named argument and breaks returns the index of it.
#' If there is no named argument, the line is broken right after the opening
#' parenthesis.
#' @inheritParams set_line_break_if_call_is_multi_line
#' @keywords internal
find_line_break_position_in_multiline_call <- function(pd) {
  candidate <- (which(pd$token == "EQ_SUB") - 1L)[1]
  ifelse(is.na(candidate), 3L, candidate)
}


#' @describeIn set_line_break_if_call_is_multi_line Sets line break before
#'   closing parenthesis.
#' @keywords internal
set_line_break_before_closing_call <- function(pd, except_token_before) {
  if (!is_function_call(pd) && !is_subset_expr(pd)) return(pd)
  npd <- nrow(pd)
  is_multi_line <- any(pd$lag_newlines[seq2(3L, npd - 1L)] > 0)
  if (!is_multi_line) {
    exception <- which(pd$token_before %in% except_token_before)
    pd$lag_newlines[setdiff(npd, exception)] <- 0L
    return(pd)
  }
  pd$lag_newlines[npd] <- 1L
  pd
}


#' @rdname set_line_break_if_call_is_multi_line
#' @keywords internal
remove_line_break_in_empty_fun_call <- function(pd) {
  if (is_function_call(pd) && nrow(pd) == 3) {
    pd$lag_newlines[3] <- 0L
  }
  pd
}
