add_ws_to_parse_data <- function(parse_data) {
  parse_data_filtered <-
    parse_data %>%
    filter_(~terminal) %>%
    select_(~-id, ~-parent, ~-terminal) %>%
    arrange_(~line1, ~col1) %>%
    bind_rows(
      tibble(line1 = 1L, col1 = 0L, line2 = 1L, col2 = 0L,
             token = "START", text = ""),
      .
    )

  parse_data_filled <-
    parse_data_filtered %>%
    create_filler

  parse_data_comment_eol <-
    parse_data_filled %>%
    mutate(text = if_else(token == "COMMENT", gsub(" +$", "", text), text))

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

create_filler <- function(data) {
  data %>%
    mutate_(
      line3 = ~lead(line1, default = tail(line2, 1)),
      col3 = ~lead(col1, default = tail(col2, 1) + 1L),
      newlines = ~line3 - line2,
      col2_nl = ~if_else(newlines > 0L, 0L, col2),
      spaces = ~col3 - col2_nl - 1
    ) %>%
    select_(~-line3, ~-col3, ~-col2_nl)
}

add_space_around_op <- function(pd) {
  op_token <- c(
    "'*'", "'/'", "'^'", "AND", "AND2", "EQ", "EQ_ASSIGN",
    "GE", "GT", "LE", "LEFT_ASSIGN", "LT", "NE", "OR", "OR2", "RIGHT_ASSIGN",
    "SPECIAL", "EQ_SUB")
  op_after <- pd$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  stopifnot(!any(op_after & op_before))
  pd$spaces[op_before & (pd$newlines == 0L)] <- 1L
  pd$spaces[op_after & (pd$newlines == 0L)] <- 1L
  pd
}

fix_quotes <- function(pd) {
  str_const <- pd$token == "STR_CONST"
  pd$text[str_const] <- gsub("^'([^'\"]*)'$", '"\\1"', pd$text[str_const])
  pd
}

remove_space_after_paren <- function(pd) {
  paren_after <- pd$token == "'('"
  pd$spaces[paren_after] <- 0L
  pd
}

remove_space_before_paren <- function(pd) {
  paren_after <- pd$token == "')'"
  paren_before <- lead(paren_after, default = FALSE)
  pd$spaces[paren_before & (pd$newlines == 0L)] <- 0L
  pd
}

add_space_after_comma <- function(pd) {
  comma_after <- pd$token == "','"
  pd$spaces[comma_after & (pd$newlines == 0L)] <- 1L
  pd
}
