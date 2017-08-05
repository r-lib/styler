# A { should never go on its own line
remove_line_break_before_curly_opening <- function(pd) {
  rm_break <- (pd$token_after == "'{'") & (pd$token != "COMMENT")
  pd$lag_newlines[lag(rm_break)] <- 0L
  pd
}

# A { should always be followed by a new line
add_line_break_afer_curly_opening <- function(pd) {
  to_break <- (pd$token == "'{'") & (pd$token_after != "COMMENT")
  pd$lag_newlines[lag(to_break)] <- 1L
  pd
}

# A } should always go on its own line, unless it’s followed by else or ).
add_line_break_before_curly_closing <- function(pd) {
  to_break <- pd$token == "'}'"
  pd$lag_newlines[to_break] <- 1L
  pd
}

# if ) follows on }, don't break line
remove_line_break_before_round_closing <- function(pd) {
  round_after_curly <- pd$token == "')'" & (pd$token_before == "'}'")
  pd$lag_newlines[round_after_curly] <- 0L
  pd
}

add_line_break_after_pipe <- function(pd) {
  is_special <- pd$token %in% c("SPECIAL-PIPE")
  pd$lag_newlines[lag(is_special)] <- 1L
  pd
}

# Break the line if
break_line_after_opening_if_call_is_multiline <- function(pd) {
  return(pd)
  npd <- nrow(pd)
  if (npd < 2) return(pd)
  if (all(is.na(pd$token_before[2]))) return(pd)
  is_call <- pd$token_before[2] == "SYMBOL_FUNCTION_CALL"
  if (!is_call) return(pd)
  if (npd == 3) {
    pd$lag_newlines[3] <- 0L
    return(pd)
  }
  comments <- which(pd$token == "COMMENT")
  is_multi_line <- any(pd$lag_newlines[3:(npd - 1)] > 0)

  if (!is_multi_line) return(pd)

  pd$lag_newlines[setdiff(3, comments)] <- 1L
  pd
}
