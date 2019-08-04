#' Check if tokens are aligned
#'
#' @details
#' A line is called aligned if the following conditions hold:
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
#' * Only invoke the function when certain that allignment is possible.
#' * Check the cheap conditions first.
#' * For the recursive creation of text, greedily check column by column to make
#'   sure we can stop as soon as we found that columns are not aligned.
#'
#' @importFrom purrr map compact reduce map_lgl map_int
#' @importFrom rlang seq2
#' @importFrom magrittr not
token_is_on_alligned_line <- function(pd_flat, op_before) {
  # works if there are no lag newlies hidden in children!
  line_idx <- 1 + cumsum(pd_flat$lag_newlines)
  pd_flat$.lag_spaces <- lag(pd_flat$spaces)
  pd_by_line <- split(pd_flat, line_idx)
  lag_spaces_col_1 <- map_int(pd_by_line, ~ .x$.lag_spaces[1])
  relevant_idx <- seq2(2, length(lag_spaces_col_1) - 1)
  relevant_lag_spaces_col_1 <- lag_spaces_col_1[relevant_idx]
  col1_is_aligned <- length(unique(relevant_lag_spaces_col_1)) == 1
  if (!col1_is_aligned) {
    return(FALSE)
  }
  has_correct_spacing_around_comma <- purrr::map_lgl(
    pd_by_line, has_correct_spacing_around_comma
  )
  if (!all(has_correct_spacing_around_comma)) {
    return(FALSE)
  }

  has_correct_spacing_around_eq_sub <- purrr::map_lgl(
    pd_by_line, has_correct_spacing_around_eq_sub
  )

  if (!all(has_correct_spacing_around_eq_sub)) {
    return(FALSE)
  }
  starting_with_comma <- purrr::map_lgl(pd_by_line, ~ .x$token[1] == "','")
  if (any(starting_with_comma)) {
    return(FALSE)
  }
  # most expensive check in the end.
  loop_upper <- max(purrr::map_int(pd_by_line, ~ sum(.x$token == "','")))
  col1_all_named <- purrr::map_lgl(pd_by_line[relevant_idx],
    ~ .x$token[c(1, 3)] == c("SYMBOL_SUB", "expr") &&
      .x$token[2] %in% c("EQ_SUB", "SPECIAL-IN", "LT", "GT", "EQ", "NE")
  ) %>%
    all()
  for (column in seq2(ifelse(col1_all_named, 1, 2), loop_upper)) {
    # check column by column since it is very expensive
    is_last <- column == loop_upper
    char_len <- purrr::map(pd_by_line[relevant_idx],
        serialize_lines, column = column,
        is_last_idx = is_last
      ) %>%
      purrr::compact() %>%
      unlist() %>%
      trimws(which = "right") %>%
      nchar()
    # TODO consistently use last()
    if (is_last && last(last(pd_by_line[relevant_idx])$token) != "','") {
      # last column does often not have a comma.
      char_len[length(char_len)] <- char_len[length(char_len)] +1
    }
    is_aligned <- length(unique(char_len)) == 1

    if (!is_aligned) {
      return(FALSE)
    }
    # for column = 2, this includes text up to the second column and space after it,
    # so it should be the same length.
    # potentially remove trainling space and compare length.
  }
  TRUE
}

serialize_lines <- function(pd,is_last_idx, column) {
  # better also add lover bound for column. If you already checked up to comma 2,
  # you don't need to re-construct text again, just check if text between comma 2
  # and 3 has the same length.

  if (is_last_idx) {
    relevant_comma <- nrow(pd)
  } else {
    comma_idx <- which(pd$token == "','")
    relevant_comma <- comma_idx[column]
    if (column > length(comma_idx)) {
      return(NULL)
    }
  }

  pd <- pd[seq2(1, nrow(pd)) < 1L + relevant_comma,]
  serialize(pd)
}

# No new lines considered
serialize <- function(pd) {

  out <- Map(function(terminal, text, child, spaces) {

    if (terminal) {
      return(paste0(text, rep_char(" ", spaces)))
    } else {
      return(paste0(serialize(child), rep_char(" ", spaces)))
    }
  }, pd$terminal, pd$text, pd$child, pd$spaces)
  paste0(out, collapse = "")
}


#' At least one space after comma, none before, for all but the last comma on
#' the line
#' @param pd_sub The subset of a parse table corresponding to one line.
#' @importFrom rlang seq2
#' @keywords internal
has_correct_spacing_around_comma <- function(pd_sub) {
  comma_tokens <- which(pd_sub$token == "','")
  if (length(comma_tokens) == 0) {
    return(TRUE)
  }
  relevant_comma_token <- comma_tokens[seq2(1, length(comma_tokens) - 1L)]
  correct_spaces_before <- pd_sub$.lag_spaces[relevant_comma_token] == 0
  correct_spaces_after <- pd_sub$spaces[relevant_comma_token] > 0
  all(correct_spaces_before) && all(correct_spaces_after)
}

#' At least one space around `EQ_SUB`
#' @keywords internal
#' @importFrom rlang seq2
has_correct_spacing_around_eq_sub <- function(pd_sub) {
  relevant_eq_sub_token <- which(pd_sub$token == "EQ_SUB")
  if (length(relevant_eq_sub_token) == 0) return(TRUE)

  correct_spaces_before <- pd_sub$.lag_spaces[relevant_eq_sub_token] >= 1
  correct_spaces_after <- pd_sub$spaces[relevant_eq_sub_token] >= 1
  all(correct_spaces_before) && all(correct_spaces_after)
}
