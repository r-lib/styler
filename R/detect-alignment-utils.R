#' Ensure the closing brace of the call is removed
#'
#' Must be after dropping comments because the closing brace is only guaranteed
#' to be the last token in that case.
#' @inheritParams alignment_drop_comments
#' @keywords internal
alignment_ensure_no_closing_brace <- function(pd_by_line,
                                              last_line_droped_early) {
  if (last_line_droped_early) {
    return(pd_by_line)
  }
  last <- last(pd_by_line)
  if (nrow(last) == 1L) {
    # can drop last line completely
    pd_by_line[-length(pd_by_line)]
  } else {
    # only drop last elment of last line
    pd_by_line[[length(pd_by_line)]] <- vec_slice(last, seq2(1L, nrow(last) - 1L))
    pd_by_line
  }
}

#' Remove all comment tokens
#'
#' Must be after split by line because it invalidates (lag)newlines, which are
#' used for splitting by line.
#' @param pd_by_line A list, each element corresponding to a potentially
#'   incomplete parse table that represents all token from one line.
#' @keywords internal
alignment_drop_comments <- function(pd_by_line) {
  map(pd_by_line, function(x) {
    out <- vec_slice(x, x$token != "COMMENT")
    if (nrow(out) < 1L) {
      return(NULL)
    }
    out
  }) %>%
    compact()
}


#' Remove last expression
#'
#' In a *nest*, if the last token is an `expr`, the *nest* represents either
#' an if, while or for statement or a function call. We don't call about that
#' part, in fact it's important to remove it for alignment. See 'Examples'.
#'
#' @examplesIf FALSE
#' call(
#'   x = 12,
#'   y =  3,
#' )
#'
#' function(a = 33,
#'          qq = 4) {
#'   # we don't care about this part for alignment detection
#' }
#' @keywords internal
alignment_drop_last_expr <- function(pds_by_line) {
  # TODO could be skipped if we know it's not a function dec
  pd_last_line <- pds_by_line[[length(pds_by_line)]]
  last_two_lines <- pd_last_line$token[c(nrow(pd_last_line) - 1L, nrow(pd_last_line))]
  if (identical(last_two_lines, c("')'", "expr"))) {
    pd_last_line <- vec_slice(pd_last_line, -nrow(pd_last_line))
  }
  pds_by_line[[length(pds_by_line)]] <- pd_last_line
  pds_by_line
}


#' Ensure last pd has a trailing comma
#'
#' Must be after [alignment_ensure_no_closing_brace()] because if it comes after
#' [alignment_ensure_trailing_comma()], the last expression would not be a
#' brace, which would make removal complicated.
#' @inheritParams alignment_drop_comments
#' @keywords internal
alignment_ensure_trailing_comma <- function(pd_by_line) {
  last_pd <- last(pd_by_line)
  # needed to make sure comma is added without space
  last_pd$spaces[nrow(last_pd)] <- 0L
  if (last(last_pd$token) == "','") {
    return(pd_by_line)
  }

  tokens <- create_tokens(
    tokens = "','",
    texts = ",",
    lag_newlines = 0L,
    spaces = 0L,
    pos_ids = NA,
    stylerignore = last_pd$stylerignore[1L],
    indents = last_pd$indent[1L]
  )
  tokens$.lag_spaces <- 0L

  tokens$lag_newlines <- tokens$pos_id <- NULL
  pd_by_line[[length(pd_by_line)]] <- rbind(last_pd, tokens)
  pd_by_line
}

#' Checks if all arguments of column 1 are named
#' @param relevant_pd_by_line A list with parse tables of a multi-line call,
#'   excluding first and last column.
#' @keywords internal
alignment_col1_all_named <- function(relevant_pd_by_line) {
  map_lgl(relevant_pd_by_line, function(x) {
    if (nrow(x) < 3L) {
      return(FALSE)
    }
    x$token[3L] == "expr" &&
      any(c("SYMBOL_SUB", "STR_CONST", "SYMBOL_FORMALS") == x$token[1L]) &&
      any(c("EQ_SUB", "EQ_FORMALS", "SPECIAL-IN", "LT", "GT", "EQ", "NE") == x$token[2L])
  }) %>%
    all()
}

#' Serialize all lines for a given column
#' @param column The index of the column to serialize.
#' @inheritParams alignment_col1_all_named
#' @keywords internal
alignment_serialize_column <- function(relevant_pd_by_line, column) {
  map(relevant_pd_by_line, alignment_serialize_line, column = column)
}

#' Serialize one line for a column
#'
#' @inheritParams alignment_serialize_column
#' @inheritParams alignment_col1_all_named
#' @keywords internal
alignment_serialize_line <- function(relevant_pd_by_line, column) {
  # TODO
  # better also add lover bound for column. If you already checked up to
  # comma 2, you don't need to re-construct text again, just check if text
  # between comma 2 and 3 has the same length.
  comma_idx <- which(relevant_pd_by_line$token == "','")
  n_cols <- length(comma_idx)
  if (column > n_cols) {
    # line does not have values at that column
    return(NULL)
  }
  between_commas <- seq2(max(1L, comma_idx[column - 1L]), comma_idx[column])
  relevant_pd_by_line <- vec_slice(relevant_pd_by_line, between_commas)
  alignment_serialize(relevant_pd_by_line)
}

#' Serialize text from a parse table
#'
#' Line breaks are ignored as they are expected to be checked in
#' [token_is_on_aligned_line()].
#' @inheritParams alignment_drop_comments
#' @keywords internal
alignment_serialize <- function(pd_sub) {
  out <- Map(function(terminal, text, child, spaces, newlines) {
    if (terminal) {
      paste0(text, rep_char(" ", spaces))
    } else {
      paste0(alignment_serialize(child), rep_char(" ", spaces))
    }
  }, pd_sub$terminal, pd_sub$text, pd_sub$child, pd_sub$spaces, pd_sub$newlines)
  if (anyNA(out)) {
    return(NA)
  }
  paste(out, collapse = "")
}

#' Check if spacing around comma is correct
#'
#' At least one space after comma, none before, for all but the last comma on
#' the line
#' @param pd_sub The subset of a parse table corresponding to one line.
#'
#' @keywords internal
alignment_has_correct_spacing_around_comma <- function(pd_sub) {
  comma_tokens <- which(pd_sub$token == "','")
  if (length(comma_tokens) == 0L) {
    return(TRUE)
  }
  relevant_comma_token <- comma_tokens[seq2(1L, length(comma_tokens) - 1L)]
  correct_spaces_before <- pd_sub$.lag_spaces[relevant_comma_token] == 0L
  correct_spaces_after <- pd_sub$spaces[relevant_comma_token] > 0L
  all(correct_spaces_before) && all(correct_spaces_after)
}

#' Check if spacing around `=` is correct
#'
#' At least one space around `EQ_SUB`
#' @inheritParams alignment_has_correct_spacing_around_comma
#' @keywords internal
alignment_has_correct_spacing_around_eq_sub <- function(pd_sub) {
  relevant_eq_sub_token <- which(pd_sub$token == "EQ_SUB")
  if (length(relevant_eq_sub_token) == 0L) {
    return(TRUE)
  }

  correct_spaces_before <- pd_sub$.lag_spaces[relevant_eq_sub_token] >= 1L
  correct_spaces_after <- pd_sub$spaces[relevant_eq_sub_token] >= 1L
  all(correct_spaces_before) && all(correct_spaces_after)
}
