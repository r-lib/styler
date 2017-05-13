compute_parse_data_flat_with_ws <- function(text) {
  parsed <- parse(text = text, keep.source = TRUE)
  parse_data <- tbl_df(utils::getParseData(parsed))
  parse_data_with_ws <- add_ws_to_parse_data(parse_data)
  parse_data_with_ws
}

#' Pre-processing parse data
#'
#' Modifies the parse table minimally by applying some pre-processing steps.
#' @details Preprocessing includes
#'   * removing non-terminal entries.
#'   * removing columns id, parent and terminal.
#'   * adding a start token.
#'   * adding linebreak and space information.
#'   * removing spaces in comments at the end of the line.
#' @param parse_data a parse table.
#' @return a pre-processed parse table.
add_ws_to_parse_data <- function(parse_data) {
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

verify_roundtrip <- function(pd, text) {
  roundtrip <- serialize_parse_data(pd)

  if (length(roundtrip) < length(text)) {
    stopifnot(text[-seq_along(roundtrip)] == "")
    text <- text[seq_along(roundtrip)]
  }

  stopifnot(identical(text, roundtrip))
  text
}

serialize_parse_data <- function(parse_data_with_ws) {
  parse_data_with_ws %>%
    summarize_(
      text_ws = ~paste0(
        text, rep_char("\n", newlines), rep_char(" ", spaces),
        collapse = "")) %>%
    .[["text_ws"]] %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
}

rep_char <- function(char, times) {
  lapply(times, rep.int, x = char) %>%
    vapply(paste, collapse = "", character(1L))
}


#' Enrich parse table with space and linebreak information
#'
#' This function computes difference (as column and line difference) between two
#'   entries in the parse table and adds this information to the table.
#' @param data a parse table.
#' @return a parse table with two new columns: newlines and spaces.
create_filler <- function(data) {
  ret <-
    data %>%
    mutate_(
      line3 = ~lead(line1, default = tail(line2, 1)),
      col3 = ~lead(col1, default = tail(col2, 1) + 1L),
      newlines = ~line3 - line2,
      col2_nl = ~if_else(newlines > 0L, 0L, col2),
      spaces = ~col3 - col2_nl - 1L
    ) %>%
    select_(~-line3, ~-col3, ~-col2_nl)

  if (any(ret$spaces < 0L)) {
    stop("Invalid parse data")
  }

  ret
}
