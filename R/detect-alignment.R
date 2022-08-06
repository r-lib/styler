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
#' withr::with_options(
#'   list(styler.cache_name = NULL), # temporarily deactivate cache
#'   {
#'     transformers <- tidyverse_style()
#'     pd_nested <- styler:::compute_parse_data_nested(c(
#'       "call(",
#'       "  ab = 1,",
#'       "  a  = 2",
#'       ")"
#'     )) %>%
#'       styler:::post_visit(transformers$initialize)
#'     nest <- pd_nested$child[[1]]
#'     styler:::token_is_on_aligned_line(nest)
#'   }
#' )
token_is_on_aligned_line <- function(pd_flat) {
  line_idx <- 1L + cumsum(pd_flat$lag_newlines)
  # cannot use lag_newlines anymore since we removed tokens
  # pos_id too expensive to construct in alignment_ensure_trailing_comma()
  pd_flat$lag_newlines <- pd_flat$pos_id <- NULL
  pd_flat$.lag_spaces <- lag(pd_flat$spaces)
  pd_by_line <- split(pd_flat, line_idx)
  pd_by_line[purrr::map_lgl(pd_by_line, ~ any(.x$stylerignore))] <- NULL
  if (length(pd_by_line) < 1) {
    return(TRUE)
  }
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
  pd_is_multi_line <- map_lgl(
    pd_by_line,
    ~ any(.x$multi_line > 0L, na.rm = TRUE)
  )
  if (any(pd_is_multi_line)) {
    return(FALSE)
  }

  pd_by_line <- alignment_drop_comments(pd_by_line)
  if (length(pd_by_line) < 1) {
    return(TRUE)
  }
  pd_by_line <- alignment_drop_last_expr(pd_by_line) %>%
    alignment_ensure_no_closing_brace(last_line_is_closing_brace_only)

  pd_by_line <- pd_by_line %>%
    alignment_ensure_trailing_comma()
  # now, pd only contains arguments separated by values, ideal for iterating
  # over columns.
  n_cols <- map_int(pd_by_line, ~ sum(.x$token == "','"))
  previous_line <- 0
  current_col <- 0
  start_eval <- ifelse(alignment_col1_all_named(pd_by_line), 1, 2)
  for (column in seq2(1, max(n_cols))) {
    by_line <- alignment_serialize_column(pd_by_line, column) %>%
      compact() %>%
      unlist() %>%
      trimws(which = "right")
    # check 1: match by comma
    # might have fewer lines in subsequent columns.
    max_previous_col <- max(current_col)

    # first col has no leading ,
    current_col <- nchar(by_line) - as.integer(column > 1)
    # Problem `by_line` counting from comma before column 3, previous_line
    # counting 1 space before ~
    if (column > 1) {
      previous_line <- previous_line[
        intersect(names(previous_line), names(by_line))
      ]
      # must add previous columns, as first column might not align
      current_col <- current_col + previous_line
    }

    is_aligned <- length(unique(current_col)) == 1L
    if (!is_aligned || length(current_col) < 2) {
      # check 2: left aligned after , (comma to next token)
      current_col <- "^(,[\\s\\t]*)[^ ]*.*$" %>%
        gsub("\\1", by_line, perl = TRUE) %>%
        nchar() %>%
        magrittr::subtract(1)

      if (column > 1) {
        # must add previous columns, as first column might not align
        current_col <- previous_line + current_col
      }
      if (length(current_col) > 1) {
        is_aligned <- length(unique(current_col)) == 1L
      } else {
        is_aligned <- current_col - max_previous_col == 1
        current_col <- max_previous_col + current_col
      }

      if (is_aligned) {
        # if left aligned after ,
        start_eval <- 2
        previous_line <- nchar(by_line) - 1 + previous_line # comma to comma
      }
    } else {
      previous_line <- current_col
    }
    if (is_aligned) {
      next
    }
    # check 3: match by = (no extra spaces around it allowed)
    # match left aligned after =
    start_after_eq <- regexpr("= [^ ]", by_line)
    names(start_after_eq) <- names(by_line)
    start_after_eq <- start_after_eq[start_after_eq > 0]

    if (column >= start_eval) {
      if (length(start_after_eq) == 0) {
        return(FALSE)
      }
      # when match via , unsuccessful, matching by = must yield at least one =
      if (column == 1) {
        current_col <- start_after_eq
      } else {
        current_col <- start_after_eq +
          previous_line[intersect(names(previous_line), names(start_after_eq))]
      }
      is_aligned <- all(
        length(unique(current_col)) == 1,
        length(start_after_eq) > 1
      )
      if (!is_aligned) {
        return(FALSE)
      }
    }
    previous_line <- nchar(by_line) + previous_line
  }
  TRUE
}
