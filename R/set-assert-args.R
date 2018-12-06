#' Set the write_tree argument
#'
#' Sets the argument `write_tree` in [test_collection()] to be `TRUE` for R
#' versions higher or equal to 3.2, and `FALSE` otherwise since the second-level
#' dependency `DiagrammeR` from `data.tree` is not available for R < 3.2.
#' @param write_tree Whether or not to write tree.
#' @keywords internal
set_arg_write_tree <- function(write_tree) {
  if (is.na(write_tree)) {
    write_tree <- ifelse(is_installed("data.tree"), TRUE, FALSE)
  } else if (write_tree) {
    assert_data.tree_installation()
  }
  write_tree
}

#' Set the file type argument
#'
#' Sets and asserts the file type argument to a standard format for further internal
#' processing.
#' @param filetype A character vector with file types to convert to the internal
#'   standard format.
#' @examples
#' styler:::set_and_assert_arg_filetype("rMd")
#' \dontrun{
#' styler:::set_and_assert_arg_filetype("xyz")
#' }
#' @keywords internal
set_and_assert_arg_filetype <- function(filetype) {
  without_dot <- gsub("^\\.", "", tolower(filetype))
  assert_filetype(without_dot)
  paste0("\\.", without_dot)
}

#' Make sure all supplied file types are allowed
#'
#' @param lowercase_filetype A vector with file types to check, all lower case.
#' @keywords internal
assert_filetype <- function(lowercase_filetype) {
  if (!all(lowercase_filetype %in% c("r", "rmd", "rnw"))) {
    stop(
      "filetype must not contain other values than 'R'",
      "'Rmd' or 'Rnw' (case is ignored).",
      call. = FALSE
    )
  }
}


#' Assert text to be of positive length and replace it with the empty
#' string otherwise.
#' @param text The input to style.
#' @keywords internal
assert_text <- function(text) {
  if (length(text) < 1) {
    text <- ""
  }
  text
}


#' Check token validity
#'
#' Check whether one or more tokens exist and have a unique token-text mapping
#' @param tokens Tokens to check.
#' @keywords internal
assert_tokens <- function(tokens) {
  invalid_tokens <- tokens[!(tokens %in% lookup_tokens()$token)]
  if (length(invalid_tokens) > 0) {
    stop(
      "Token(s) ", paste0(invalid_tokens, collapse = ", "), " are invalid. ",
      "You can lookup all valid tokens and their text ",
      "with styler:::lookup_tokens(). Make sure you supply the values of ",
      "the column 'token', not 'text'."
    )
  }
}
