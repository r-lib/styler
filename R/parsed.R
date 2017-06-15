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
  parsed <- parse(text = text, keep.source = TRUE)
  parse_data <- tbl_df(utils::getParseData(parsed))
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
    select_(~-id, ~-parent, ~-terminal) %>%
    arrange_(~line1, ~col1, ~line2, ~col2) %>%
    bind_rows(
      tibble(line1 = 1L, col1 = 0L, line2 = 1L, col2 = 0L,
             token = "START", text = ""),
      .
    )

  parse_data_filled <-
    parse_data_filtered %>%
    create_filler()

  parse_data_comment_eol <-
    parse_data_filled %>%
    mutate_(text = ~if_else(token == "COMMENT", gsub(" +$", "", text), text))

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

#' Serialize Flat Parse Data
#'
#' Collapses a parse table into character vector representation.
#' @param pd_flat A parse table.
#' @details
#'   The function essentially collapses the column text of `pd_flat`
#'   while taking into account space and linebreak information from the columns
#'   newlines and spaces. \cr
#'   Roughly speaking, this is the inverse operation of
#'   [compute_parse_data_flat_enhanced()], which turns a character vector into a
#'   parse table, since `serialize_parse_data_flat()` turns a parse table back
#'   into a character vector.
serialize_parse_data_flat <- function(pd_flat) {
  pd_flat %>%
    summarize_(
      text_ws = ~paste0(
        text, newlines_and_spaces(newlines, spaces),
        collapse = "")) %>%
    .[["text_ws"]] %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
}


#' Enrich parse table with space and linebreak information
#'
#' This function computes difference (as column and line difference) between two
#'   entries in the parse table and adds this information to the table.
#' @param pd_flat A parse table.
#' @return A parse table with two three columns: lag_newlines, newlines and
#'   spaces.
#' @seealso [create_filler_nested()]
create_filler <- function(pd_flat) {
  ret <-
    pd_flat %>%
    mutate_(
      line3 = ~lead(line1, default = tail(line2, 1)),
      col3 = ~lead(col1, default = tail(col2, 1) + 1L),
      newlines = ~line3 - line2,
      lag_newlines = ~lag(newlines, default = 0),
      col2_nl = ~if_else(newlines > 0L, 0L, col2),
      spaces = ~col3 - col2_nl - 1L
    ) %>%
    select_(~-line3, ~-col3, ~-col2_nl)

  if (any(ret$spaces < 0L)) {
    stop("Invalid parse data")
  }

  ret
}

