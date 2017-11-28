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
#' operating system.
#' @param sys_call The call to be executed.
#' @param ... Arguments passed to [shell()] or [system()].
calls_sys <- function(sys_call, ...) {
  if (Sys.info()[1] == "Windows") {
    error <- shell(sys_call, ...)
  } else {
    error <- system(sys_call, ...)
  }
}

#' Assert text to be of positive length and replace it with the empty
#' string otherwise.
#' @param text The input to style.
assert_text <- function(text) {
  if (length(text) < 1) {
    text <- ""
  }
  text
}

is_plain_r_file <- function(path) {
  grepl("\\.R$", path, ignore.case = TRUE)
}

is_rmd_file <- function(path) {
  grepl("\\.Rmd$", path, ignore.case = TRUE)
}

is_unsaved_file <- function(path) {
  path == ""
}

#' Find the index of the next non-comment in a parse table
#' @param pd A parse table.
#' @param pos The position of the token to start the search from.
#' @importFrom rlang seq2
next_non_comment <- function(pd, pos) {
  if (length(pos) < 1 || is.na(pos) || pos >= nrow(pd)) return(integer(0))
  candidates <- seq2(pos + 1L, nrow(pd))
  if (all(candidates %in% which(pd$token == "COMMENT"))) return(integer(0))
  setdiff(candidates, which(pd$token == "COMMENT"))[1]
}

#' Find the index of the last comment in the sequence of comments-only tokens
#' after the token that has position `pos` in `pd`.
#' @param pd A parse table.
#' @param pos The position of the token to start the search from.
extend_if_comment <- function(pd, pos) {
  if (pos == nrow(pd)) return(pos)
  if (pd$token[pos + 1] == "COMMENT") {
    extend_if_comment(pd, pos + 1L)
  } else {
    pos
  }
}
