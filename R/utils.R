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

#' Combine child and internal child
#'
#' binds two parse tables together and arranges them so that the tokens are in
#'   the correct order.
#' @param child A parse table or `NULL`.
#' @param internal_child A parse table or `NULL`.
#' @details Essentially, this is a wrapper around [dplyr::bind_rows()], but
#'   returns `NULL` if the result of [dplyr::bind_rows()] is a data frame with
#'   zero rows.
combine_children <- function(child, internal_child) {
  bound <- bind_rows(child, internal_child)
  if (nrow(bound) == 0) return(NULL)
  arrange_(bound,  ~line1, ~col1)
}
