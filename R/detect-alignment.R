#' Check if tokens are aligned
#'
#' If all tokens are aligned, `TRUE` is returned, otherwise `FALSE`. The
#' function only checks for alignment of function calls. This can be
#' recycled conveniently later if needed as a vector with length > 1.
#' @param pd_flat A flat parse table.
#' @details
#' Multiple lines are called aligned if the following conditions hold for all
#' but the first line of the expression:
#'
#' * lag spaces of column 1 must agree.
#' * spacing around comma (0 before, > 1 after) and spacing around `=` (at least
#'   one around).
#' * all positions of commas of col > 2 must agree (needs recursive creation of
#'   `text`).
#'
#' Because of the last requirement, this function is very expensive to run. For
#' this reason, the following approach is taken:
#'
#' * Only invoke the function when certain that alignment is possible.
#' * Check the cheap conditions first.
#' * For the recursive creation of text, greedily check column by column to make
#'   sure we can stop as soon as we found that columns are not aligned.
#'
#' @importFrom purrr map_int map_lgl map compact
#' @importFrom rlang seq2
#' @keywords internal
#' @examples
#' library("magrittr")
#' transformers <- tidyverse_style()
#' pd_nested <- styler:::compute_parse_data_nested(c(
#'   "call(",
#'   "  ab = 1,",
#'   "  a  = 2",
#'   ")"
#' )) %>%
#' styler:::post_visit(transformers$initialize)
#' nest <- pd_nested$child[[1]]
#' styler:::token_is_on_aligned_line(nest)
token_is_on_aligned_line <- function(pd_flat) {

  line_idx <- 1 + cumsum(pd_flat$lag_newlines)
  pd_flat$.lag_spaces <- lag(pd_flat$spaces)
  pd_by_line <- split(pd_flat, line_idx)
  last_line_is_closing_brace_only <- nrow(last(pd_by_line)) == 1
  relevant_idx <- seq2(2, ifelse(last_line_is_closing_brace_only,
    length(pd_by_line) - 1,
    length(pd_by_line)
  ))
  pd_by_line <- pd_by_line[relevant_idx]

  relevant_lag_spaces_col_1 <- map_int(pd_by_line, ~ .x$.lag_spaces[1])

  col1_is_aligned <- length(unique(relevant_lag_spaces_col_1)) == 1
  if (!col1_is_aligned) {
    return(FALSE)
  }
  has_correct_spacing_around_comma <- map_lgl(
    pd_by_line, alignment_has_correct_spacing_around_comma
  )
  if (!all(has_correct_spacing_around_comma)) {
    return(FALSE)
  }

  has_correct_spacing_around_eq_sub <- map_lgl(
    pd_by_line, alignment_has_correct_spacing_around_eq_sub
  )

  if (!all(has_correct_spacing_around_eq_sub)) {
    return(FALSE)
  }
  starting_with_comma <- map_lgl(pd_by_line, ~ .x$token[1] == "','")
  if (any(starting_with_comma)) {
    return(FALSE)
  }
  pd_is_multi_line <- map_lgl(pd_by_line, ~ any(.x$multi_line, na.rm = TRUE))
  if (any(pd_is_multi_line)) {
    return(FALSE)
  }

  pd_by_line <- alignment_drop_comments(pd_by_line) %>%
    alignment_ensure_no_closing_brace(last_line_is_closing_brace_only) %>%
    alignment_ensure_trailing_comma()
  # now, pd only contains arguments separated by values, ideal for iterating
  # over columns.
  # cannot use lag_newlines anymore since we removed tokens.
  pd_by_line <- map(pd_by_line, function(pd_sub) {
    pd_sub$lag_newlines <- NULL
    pd_sub
  })

  n_cols <- map_int(pd_by_line, ~ sum(.x$token == "','"))
  start <- ifelse(all(alignment_col1_is_named(pd_by_line)), 1, 2)

  for (column in seq2(start, max(n_cols))) {
    char_len <- alignment_serialize_column(pd_by_line, column) %>%
      compact() %>%
      unlist() %>%
      trimws(which = "right") %>%
      nchar()

    is_aligned <- length(unique(char_len)) == 1

    if (!is_aligned) {
      return(FALSE)
    }
  }
  TRUE
}
