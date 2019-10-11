#' Repeat elements of a character vector `times` times and collapse it
#'
#' @param char A character vector.
#' @param times an integer giving the number of repetitions.
#' @return A character vector.
#' @keywords internal
rep_char <- function(char, times) {
  paste(rep.int(char, times), collapse = "")
}

#' Concentrate newlines or spaces in a string
#'
#' @param n Scalar indicating how many characters should be concentrated
#' @return A string.
#' @name add_spaces_or_newlines
#' @keywords internal
NULL

#' @rdname add_spaces_or_newlines
#' @keywords internal
add_newlines <- function(n) {
  rep_char("\n", n)
}

#' @rdname add_spaces_or_newlines
#' @keywords internal
add_spaces <- function(n) {
  rep_char(" ", n)
}


has_one_backslash <- function(x) {
  x <- char2raw_as_char(x)
  any(substr(x, 1, 1) == "0")
}

has_multiple_backslashes <- function(x) {
  x <- char2raw_as_char(x)
  any(x == "5c")
}

char2raw_as_char <- function(x) {
  x <- charToRaw(x)
  class(x) <- "character"
  x
}

