nest_parse_data <- function(parse_data) {
  if (nrow(parse_data) <= 1) return(parse_data)
  split <-
    parse_data %>%
    mutate_(internal = ~id %in% parent) %>%
    nest_("data", names(parse_data))

  leaves <- split$data[!split$internal][[1L]]
  internal <- split$data[split$internal][[1L]]

  if ("leaves" %in% names(internal)) {
    internal <- rename_(internal, internal_leaves = ~leaves)
  } else {
    internal <- mutate_(internal, internal_leaves = ~vector("list", nrow(internal)))
  }

  nested <-
    leaves %>%
    mutate_(parent_ = ~parent) %>%
    nest_(., "leaves", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_")) %>%
    mutate_(leaves = ~Map(bind_rows, leaves, internal_leaves)) %>%
    mutate_(leaves = ~lapply(leaves, arrange_, ~line1, ~col1)) %>%
    select_(~-internal_leaves) %>%
    select_(~short, ~everything(), ~-text, ~text)

  nest_parse_data(nested)
}

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
    create_filler

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

op_token <- c(
  "'+'", "'-'", "'*'", "'/'", "'^'", "AND", "AND2", "EQ", "EQ_ASSIGN",
  "GE", "GT", "LE", "LEFT_ASSIGN", "LT", "NE", "OR", "OR2", "RIGHT_ASSIGN",
  "SPECIAL", "EQ_SUB", "ELSE")

add_space_around_op <- function(pd) {
  op_after <- pd$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd$newlines == 0L)
  pd$spaces[idx_before] <- pmax(pd$spaces[idx_before], 1L)
  idx_after <- op_after & (pd$newlines == 0L)
  pd$spaces[idx_after] <- pmax(pd$spaces[idx_after], 1L)
  pd
}

set_space_around_op <- function(pd) {
  op_after <- pd$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  pd$spaces[op_before & (pd$newlines == 0L)] <- 1L
  pd$spaces[op_after & (pd$newlines == 0L)] <- 1L
  pd
}

remove_space_after_unary_pm <- function(pd) {
  op_pm <- c("'+'", "'-'")
  op_pm_unary_after <- c(op_pm, op_token, "'('", "','")

  pm_after <- pd$token %in% op_pm
  pd$spaces[pm_after & (pd$newlines == 0L) & dplyr::lag(pd$token) %in% op_pm_unary_after] <- 0L
  pd
}

fix_quotes <- function(pd) {
  str_const <- pd$token == "STR_CONST"
  str_const_change <- grepl("^'([^\"]*)'$", pd$text[str_const])
  pd$text[str_const][str_const_change] <-
    vapply(
      lapply(pd$text[str_const][str_const_change], parse_text),
      deparse,
      character(1L))
  pd
}

remove_space_before_opening_paren <- function(pd) {
  paren_after <- pd$token == "'('"
  paren_before <- lead(paren_after, default = FALSE)
  pd$spaces[paren_before & (pd$newlines == 0L)] <- 0L
  pd
}

remove_space_after_opening_paren <- function(pd) {
  paren_after <- pd$token == "'('"
  pd$spaces[paren_after & (pd$newlines == 0L)] <- 0L
  pd
}

remove_space_before_closing_paren <- function(pd) {
  paren_after <- pd$token == "')'"
  paren_before <- lead(paren_after, default = FALSE)
  pd$spaces[paren_before & (pd$newlines == 0L)] <- 0L
  pd
}

add_space_after_for_if_while <- function(pd) {
  comma_after <- pd$token %in% c("FOR", "IF", "WHILE")
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[idx] <- pmax(pd$spaces[idx], 1L)
  pd
}

add_space_before_brace <- function(pd) {
  op_after <- pd$token %in% "'{'"
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd$newlines == 0L) & pd$token != "'('"
  pd$spaces[idx_before] <- pmax(pd$spaces[idx_before], 1L)
  pd
}

add_space_after_comma <- function(pd) {
  comma_after <- pd$token == "','"
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[idx] <- pmax(pd$spaces[idx], 1L)
  pd
}

set_space_after_comma <- function(pd) {
  comma_after <- pd$token == "','"
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[comma_after & (pd$newlines == 0L)] <- 1L
  pd
}
