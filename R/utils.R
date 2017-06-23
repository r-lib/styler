parse_text <- function(x) parse(text = x)[[1L]]

#' Repeat elements of a character vector `times` times and collapse it
#'
#' @param char A character vector.
#' @param times an integer giving the number of repetitions.
#' @return A character vector.
rep_char <- function(char, times) {
  lapply(times, rep.int, x = char) %>%
    vapply(paste, collapse = "", character(1L))
}

#' concentrate newlines an spaces in a string
#'
#' @param newlines Scalar indicating how many newlines ("\ n") should returned.
#' @param spaces Scalar indicating how many spaces should be appended to the
#'   newlines.
#' @return A string.
newlines_and_spaces <- function(newlines, spaces) {
  paste0(rep_char("\n", newlines), rep_char(" ", spaces))
}

add_newlines <- function(n) {
  rep_char("\n", n)
}

add_spaces <- function(n) {
  rep_char(" ", n)
}
