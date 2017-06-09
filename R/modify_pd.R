indent_round <- function(pd, indent_by) {
  start <- which(pd$token == "'('") + 1
  stop <- which(pd$token == "')'") - 1
  if (length(start) == 0 && length(stop) == 0) {
    pd$indent <- 0
  } else {
    pd$indent <- ifelse(1:nrow(pd) %in% start[1]:stop[1], indent_by, 0) *
      lag(pd$newlines, default = 0)
    pd$spaces <- pd$spaces * (pd$newlines == 0)
  }
  pd$lag_newlines <- lag(pd$newlines, default = 0)
  select_(pd, ~indent, ~newlines, ~lag_newlines, ~everything())
}


indent_round_nested <- function(pd) {
  if (is.null(pd)) return(pd)
  pd <- indent_round(pd, indent_by = 2)
  pd$child <- map(pd$child, indent_round_nested)
  pd
}


# update terminal line info
previous_terminal_line_nested <- function(pd) {
  if (is.null(pd)) return()
  if (max(pd$line2) - min(pd$line1) < 1) {
    pd$terminal_line <- c(1, rep(0, nrow(pd) - 1))
    return(pd)
  }
  pd$child <- map(pd$child, previous_terminal_line_nested)
  pd
}

introduce_previous_terminal_line <- function(pd) {
  pd$terminal_line <- 0
  pd
}


introduce_previous_terminal_line_nested <- function(pd) {
  pd <- introduce_previous_terminal_line(pd)
  pd$child <- map(pd$child, introduce_previous_terminal_line)
  pd
}
