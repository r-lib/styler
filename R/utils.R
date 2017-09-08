parse_text <- function(x) parse(text = x)[[1L]]

#' Repeat elements of a character vector `times` times and collapse it
#'
#' @param char A character vector.
#' @param times an integer giving the number of repetitions.
#' @return A character vector.
rep_char <- function(char, times) {
  paste(rep.int(char, times), collapse = "")
}

#' Concentrate newlines or spaces in a string
#'
#' @param n Scalar indicating how many characters should be concentrated
#' @return A string.
#' @name add_spaces_or_newlines
NULL

#' @rdname add_spaces_or_newlines
add_newlines <- function(n) {
  rep_char("\n", n)
}

#' @rdname add_spaces_or_newlines
add_spaces <- function(n) {
  rep_char(" ", n)
}

#' Invoke a system command
#'
#' Wraps a system command into [shell()] or [system()], depending on the
#' os.
#' @param sys_call The call to be executed.
#' @param ... Arguments passed to [shell()] or [system()].
calls_sys <- function(sys_call, ...) {
  if (Sys.info()[1] == "Windows") {
    error <- shell(sys_call, ...)
  } else {
    error <- system(sys_call, ...)
  }
}
