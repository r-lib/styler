parse_text <- function(x) parse_safely(x)[[1L]]

line_col_names <- function() {
  c("line1", "line2", "col1", "col2")
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
  if (length(x) < 1) return(NULL)
  seq(1L, length(x), by = 2)
}

even <- function(x) {
  if (length(x) < 2) return(NULL)
  x[even_index(x)]
}

even_index <- function(x) {
  seq(2L, length(x), by = 2)
}

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

is_plain_r_file <- function(path) {
  grepl("\\.R$", path, ignore.case = TRUE)
}

is_rmd_file <- function(path) {
  grepl("\\.Rmd$", path, ignore.case = TRUE)
}

is_rnw_file <- function(path) {
  grepl("\\.Rnw$", path, ignore.case = TRUE)
}

is_unsaved_file <- function(path) {
  path == ""
}

#' Find the index of the next non-comment in a parse table
#' @param pd A parse table.
#' @param pos The position of the token to start the search from.
#' @importFrom rlang seq2
#' @keywords internal
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
#' @keywords internal
extend_if_comment <- function(pd, pos) {
  if (pos == nrow(pd)) return(pos)
  if (pd$token[pos + 1] == "COMMENT") {
    extend_if_comment(pd, pos + 1L)
  } else {
    pos
  }
}

#' Map the file type to a corresponding regular expression
#'
#' @param filetype The file type to map to a regex.
#' @examples
#' styler:::map_filetype_to_pattern(c(".rMd", "R"))
#' @keywords internal
map_filetype_to_pattern <- function(filetype) {
  paste0("(", paste(set_and_assert_arg_filetype(filetype), collapse = "|"), ")$")
}
