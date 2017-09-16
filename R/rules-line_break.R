# A { should never go on its own line
remove_line_break_before_curly_opening <- function(pd) {
  rm_break <- (pd$token_after == "'{'") & (pd$token != "COMMENT")
  pd$lag_newlines[lag(rm_break)] <- 0L
  pd
}

# A { should always be followed by a new line
set_line_break_afer_curly_opening <- function(pd) {
  to_break <- (pd$token == "'{'") & (pd$token_after != "COMMENT")
  pd$lag_newlines[lag(to_break)] <- 1L
  pd
}

add_line_break_afer_curly_opening <- function(pd) {
  to_break <- lag(
    (pd$token == "'{'") & (pd$token_after != "COMMENT"),
    default = FALSE
  )
  pd$lag_newlines[to_break] <- pmax(1L, pd$lag_newlines[to_break])
  pd
}


# A } should always go on its own line, unless it’s followed by else or ).
set_line_break_before_curly_closing <- function(pd) {
  to_break <- pd$token == "'}'"
  pd$lag_newlines[to_break] <- 1L
  pd
}

add_line_break_before_curly_closing <- function(pd) {
  to_break <- pd$token == "'}'"
  pd$lag_newlines[to_break] <- pmax(1L, pd$lag_newlines[to_break])
  pd
}

# if ) follows on }, don't break line
remove_line_break_before_round_closing <- function(pd) {
  round_after_curly <- pd$token == "')'" & (pd$token_before == "'}'")
  pd$lag_newlines[round_after_curly] <- 0L
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
NULL

#' @describeIn set_line_break_if_call_is_multi_line Sets line break after
#'   opening parenthesis.
set_line_break_after_opening_if_call_is_multi_line <-
  function(pd,
           except_token_after = NULL,
           except_text_before  = NULL) {
  if (!is_function_call(pd)) return(pd)
  npd <- nrow(pd)
  seq_x <- seq2(3, npd - 1)
  is_multi_line <- any(
    (pd$lag_newlines[seq_x] > 0) |
    (pd$token[seq_x] == "COMMENT")
  )
  if (!is_multi_line) {
    return(pd)
  }
  exception_pos <- c(
    which(pd$token %in% except_token_after),
    ifelse(pd$child[[1]]$text[1] %in% except_text_before, 3L, NA)
  )
  pd$lag_newlines[setdiff(3, exception_pos)] <- 1L
  pd
}


#' @describeIn set_line_break_if_call_is_multi_line Sets line break before
#'   closing parenthesis.
set_line_break_before_closing_call <- function(pd, except_token_before) {
  if (!is_function_call(pd)) return(pd)
  npd <- nrow(pd)
  is_multi_line <- any(pd$lag_newlines[seq2(3, npd - 1)] > 0)
  if (!is_multi_line) {
    exception <- which(pd$token_before %in% except_token_before)
    pd$lag_newlines[setdiff(npd, exception)] <- 0L
    return(pd)
  }
  pd$lag_newlines[npd] <- 1L
  pd
}


#' @rdname set_line_break_if_call_is_multi_line
remove_line_break_in_empty_fun_call <- function(pd) {
  if (is_function_call(pd) && nrow(pd) == 3) {
    pd$lag_newlines[3] <- 0L
  }
  pd
}
