#' Check if tokens are aligned
#'
#' If all tokens are aligned, `TRUE` is returned, otherwise `FALSE`.
#' @param pd_flat A flat parse table.
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
#' * Only invoke the function when certain that alignment is possible.
#' * Check the cheap conditions first.
#' * For the recursive creation of text, greedily check column by column to make
#'   sure we can stop as soon as we found that columns are not aligned.
#'
#' @importFrom purrr map compact reduce map_lgl map_int
#' @importFrom rlang seq2
#' @importFrom magrittr not
token_is_on_aligned_line <- function(pd_flat) {
  line_idx <- 1 + cumsum(pd_flat$lag_newlines)
  pd_flat$.lag_spaces <- lag(pd_flat$spaces)
  pd_by_line <- split(pd_flat, line_idx)
  # cannot use lag_newlines and newlines anymore since we removed tokens. Need
  # to remove comments because code will fail if last column is comment only.
  last_line_is_closing_brace_only <- nrow(last(pd_by_line)) == 1
  relevant_idx <- seq2(2, ifelse(last_line_is_closing_brace_only,
    length(pd_by_line) - 1,
    length(pd_by_line)
  )) # always remove first line, also e.g. map(x, f,\nx = 2)
  pd_by_line <- pd_by_line[relevant_idx]


  lag_spaces_col_1 <- map_int(pd_by_line, ~ .x$.lag_spaces[1])

  relevant_lag_spaces_col_1 <- lag_spaces_col_1
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

  pd_by_line <- alignment_drop_comments(pd_by_line) %>%
    alignment_ensure_no_closing_brace(last_line_is_closing_brace_only) %>%
    alignment_ensure_trailing_comma()
  # now, pd only contains arguments separated by values, ideal for iterating
  # over columns

  n_cols <- purrr::map_int(pd_by_line, ~ sum(.x$token == "','"))
  start <- ifelse(all(col1_is_named(pd_by_line)), 1, 2)

  for (column in seq2(start, max(n_cols))) {
    # check column by column since it is very expensive
    char_len <- serialize_column(pd_by_line, column) %>%
      purrr::compact() %>%
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

#' Must be after dropping comments because the closing brace is only guaranteed
#' to be the last token in that case.
alignment_ensure_no_closing_brace <- function(pd_by_line, last_line_droped_early) {
  if (last_line_droped_early) {
    return(pd_by_line)
  }
  last <- last(pd_by_line)
  if (nrow(last) == 1) {
    # can drop last line completely
    pd_by_line[-length(pd_by_line)]
  } else {
    # only drop last elment of last line
    pd_by_line[[length(pd_by_line)]] <- last[seq2(1, nrow(last) - 1),]
    pd_by_line
  }

}


#' Remove all comment tokens
#'
#' Must be after split by line because it invalidates (lag)newlines, which are
#' used for splitting by line.
alignment_drop_comments <- function(pd_by_line) {
  purrr::map(pd_by_line, function(x) {
    out <- x[x$token != "COMMENT",]
    if (nrow(out) < 1) {
      return(NULL)
    } else {
      out
    }
  }) %>%
    purrr::compact()
}

#' Must be after [alignment_drop_closing_brace()] because if it comes after
#' [alignment_ensure_trailing_comma()], the last expression would not be a
#' brace, which would make removal complicated.
alignment_ensure_trailing_comma <- function(pd_by_line) {
  last_pd <- last(pd_by_line)
  # needed to make sure comma is aded without space
  last_pd$spaces[nrow(last_pd)] <- 0
  if (last(last_pd$token) == "','") {
    return(pd_by_line)
  } else {
    pos_id <- create_pos_ids(last_pd, nrow(last_pd), after = TRUE)
    tokens <- create_tokens(
      tokens = "','",
      texts = ",",
      lag_newlines = 0,
      spaces = 0,
      pos_ids = pos_id,
      )
    tokens$.lag_spaces <- 0
    pd_by_line[[length(pd_by_line)]] <- rbind(last_pd, tokens)
    pd_by_line
  }
}

#' Checks if all arguments of column 1 are named
#' @param relevant_pd_by_line A list with parse tables of a multi-line call,
#'   excluding first and last column.
#' @keywords internal
col1_is_named <- function(relevant_pd_by_line) {
  purrr::map_lgl(relevant_pd_by_line, function(x) {
    x$token[c(1, 3)] == c("SYMBOL_SUB", "expr") &&
      x$token[2] %in% c(
        "EQ_SUB", "SPECIAL-IN", "LT", "GT", "EQ", "NE"
      )
  }) %>%
    all()
}

#' Serialize all lines for a given column
#' @param colum Which column to serialize.
#' @inheritParams col1_is_named
#' @keywords internal
serialize_column <- function(relevant_pd_by_line, column) {
  purrr::map2(
    relevant_pd_by_line,
    c(rep(FALSE, length(relevant_pd_by_line) - 1), TRUE),
    serialize_line,
    column = column
  )
}

#' Serialize one line for a column
#'
#' @param is_last_line Boolean for every element of `relevant_pd_by_line`
#'   indicating if it is the last line.
#' @param column The index of the column to serialize.
#' @inheritParams col1_is_named
serialize_line <- function(relevant_pd_by_line, is_last_line, column) {
  # better also add lover bound for column. If you already checked up to comma 2,
  # you don't need to re-construct text again, just check if text between comma 2
  # and 3 has the same length.
  comma_idx <- which(relevant_pd_by_line$token == "','")
  n_cols <- length(comma_idx)
  if (column > n_cols) {
    # line does not have values at that column
    return(NULL)
  } else {
    relevant_comma <- comma_idx[column]
  }

  relevant_pd_by_line <- relevant_pd_by_line[seq2(1, relevant_comma), ]
  serialize(relevant_pd_by_line)
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


#' Check if spacing around comma is correcr
#'
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

#' Check if spacing around `=` is correct
#'
#' At least one space around `EQ_SUB`
#' @inheritParams has_correct_spacing_around_comma
#' @keywords internal
#' @importFrom rlang seq2
has_correct_spacing_around_eq_sub <- function(pd_sub) {
  relevant_eq_sub_token <- which(pd_sub$token == "EQ_SUB")
  if (length(relevant_eq_sub_token) == 0) {
    return(TRUE)
  }

  correct_spaces_before <- pd_sub$.lag_spaces[relevant_eq_sub_token] >= 1
  correct_spaces_after <- pd_sub$spaces[relevant_eq_sub_token] >= 1
  all(correct_spaces_before) && all(correct_spaces_after)
}
