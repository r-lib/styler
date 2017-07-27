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
