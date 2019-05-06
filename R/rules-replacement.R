force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}


resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) {
    return(pd)
  }
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}
