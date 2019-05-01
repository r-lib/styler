parse_text <- function(x) parse_safely(x)[[1L]]

line_col_names <- function() {
  c("line1", "line2", "col1", "col2")
}

ensure_last_is_empty <- function(x) {
  has_line_break_at_eof <- x[length(x)] == ""
  if (has_line_break_at_eof) {
    return(x)
  } else {
    append(x, "")
  }
}

#' Check whether two columns match
#'
#' @param col1,col2 Column names as string.
#' @param data The data frames that contains `col1` and `col2`.
#' @keywords internal
two_cols_match <- function(col1, col2, data) {
  all(unlist(data[col1]) == unlist(data[col2]))
}

odd <- function(x) {
  x[odd_index(x)]
}

odd_index <- function(x) {
  if (length(x) < 1) {
    return(NULL)
  }
  seq(1L, length(x), by = 2)
}

even <- function(x) {
  if (length(x) < 2) {
    return(NULL)
  }
  x[even_index(x)]
}

even_index <- function(x) {
  seq(2L, length(x), by = 2)
}


#' Invoke a system command
#'
#' Wraps a system command into [shell()] or [system()], depending on the
#' operating system.
#' @param sys_call The call to be executed.
#' @param ... Arguments passed to [shell()] or [system()].
#' @keywords internal
calls_sys <- function(sys_call, ...) {
  if (Sys.info()[1] == "Windows") {
    error <- shell(sys_call, ...)
  } else {
    error <- system(sys_call, ...)
  }
}
