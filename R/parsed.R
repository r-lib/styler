#' Parse and pre-process character vector
#'
#' @description The function obtains detailed parse information for `text` via
#'   [utils::getParseData()] and does some minimal pre-processing by calling
#'   [enhance_parse_data()].
#' @inheritSection enhance_parse_data Details
#' @param text A character vector.
#' @return A pre-processed parse table.
#' @details Roughly speaking, this is the inverse operation of
#'   [serialize_parse_data_flat()], which turns a parse table into a character
#'   vector, since `compute_parse_data_flat_enhanced()` turns a character vector
#'   into a parse table.
compute_parse_data_flat_enhanced <- function(text) {
  parse_data <- tokenize(text)
  pd_flat <- enhance_parse_data(parse_data)
  pd_flat
}

#' Pre-processing parse data
#'
#' Modifies the parse table minimally by applying some pre-processing steps.
#' @section Details:
#' Preprocessing includes
#'   * removing non-terminal entries.
#'   * removing columns id, parent and terminal.
#'   * adding a start token.
#'   * adding line-break and space information.
#'   * removing spaces in comments at the end of the line.
#' @param parse_data a parse table.
#' @return a pre-processed parse table.
enhance_parse_data <- function(parse_data) {
  parse_data_filtered <-
    parse_data %>%
    filter_(~terminal) %>%
    select_(~-id, ~-parent) %>%
    arrange_(~line1, ~col1, ~line2, ~col2) %>%
    bind_rows(
      tibble(line1 = 1L, col1 = 0L, line2 = 1L, col2 = 0L,
             token = "START", text = ""),
      .
    )

  parse_data_filled <-
    parse_data_filtered %>%
    create_filler()

  parse_data_comment_eol <- parse_data_filled

  parse_data_comment_eol$text <-
    if_else(parse_data_comment_eol$token == "COMMENT",
            gsub(" +$", "", parse_data_comment_eol$text),
            parse_data_comment_eol$text)

  parse_data_comment_eol
}

#' Verify parse data modifications
#'
#' @description Check whether serializing the parse data results in the same
#' number of lines as the initial data that should be styled.
#' @param pd_flat A parse table.
#' @param text A character vector with the initial text to compare against.
#' @return If the verification is successful, `pd` is returned, with empty
#'   lines at the end of `text` stripped. \cr
#'   Otherwise, an error is thrown.
#' @seealso [serialize_parse_data_flat()]
verify_roundtrip <- function(pd_flat, text) {
  roundtrip <- serialize_parse_data_flat(pd_flat)

  if (length(roundtrip) < length(text)) {
    stopifnot(text[-seq_along(roundtrip)] == "")
    text <- text[seq_along(roundtrip)]
  }

  stopifnot(identical(text, roundtrip))
  text
}

#' Enrich parse table with space and linebreak information
#'
#' This function computes difference (as column and line difference) between two
#'   entries in the parse table and adds this information to the table.
#' @param pd_flat A parse table.
#' @return A parse table with two three columns: lag_newlines, newlines and
#'   spaces.
create_filler <- function(pd_flat) {

  pd_flat$line3 <- lead(pd_flat$line1, default = tail(pd_flat$line2, 1))
  pd_flat$col3 <- lead(pd_flat$col1, default = tail(pd_flat$col2, 1) + 1L)
  pd_flat$newlines <- pd_flat$line3 - pd_flat$line2
  pd_flat$lag_newlines <- lag(pd_flat$newlines, default = 0L)
  pd_flat$col2_nl <- if_else(pd_flat$newlines > 0L, 0L, pd_flat$col2)
  pd_flat$spaces <- pd_flat$col3 - pd_flat$col2_nl - 1L
  pd_flat$multi_line <- ifelse(pd_flat$terminal, FALSE, NA)

  ret <- pd_flat[, !(names(pd_flat) %in% c("line3", "col3", "col2_nl"))]


  if (!("indent" %in% names(ret))) {
    ret$indent <- 0
  }

  if (any(ret$spaces < 0L)) {
    stop("Invalid parse data")
  }

  ret
}

