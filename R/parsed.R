apply_text_transformers_on_file <- function(file, transformers) {
  message(file)

  text <- readLines(file, warn = FALSE)
  text <- gsub(" +$", "", text)
  text <- gsub("\t", "        ", text)

  parsed <- parse(file, keep.source = TRUE)
  parse_data <- utils::getParseData(parsed)
  parse_data_with_ws <- add_ws_to_parse_data(parse_data)
  roundtrip <- serialize_parse_data(parse_data_with_ws)

  if (length(roundtrip) < length(text)) {
    stopifnot(text[-seq_along(roundtrip)] == "")
    text <- text[seq_along(roundtrip)]
  }

  stopifnot(all(text == roundtrip))

  transformed_data_with_ws <- Reduce(
    function(x, fun) fun(x),
    transformers,
    init = parse_data_with_ws)

  new_text <- serialize_parse_data(transformed_data_with_ws)

  if (any(text != new_text)) {
    writeLines(new_text, file)
    TRUE
  } else {
    FALSE
  }
}

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
    mutate_(filler = ~create_filler(
      line2, col2,
      lead(line1, default = tail(line2, 1)),
      lead(col1, default = tail(col2, 1) + 1L)))

  parse_data_comment_eol <-
    parse_data_filled %>%
    mutate(text = if_else(token == "COMMENT", gsub(" +$", "", text), text))

  parse_data_comment_eol
}

serialize_parse_data <- function(parse_data_with_ws) {
  paste0(parse_data_with_ws$text, parse_data_with_ws$filler, collapse = "") %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
}

create_filler <- function(line2, col2, line3, col3) {
  line_diff <- line3 - line2
  nl <- line_diff > 0L
  col2[nl] <- 0L
  filler <-
    lapply(col3 - col2 - 1, rep.int, x = " ") %>%
    vapply(paste, collapse = "", character(1))
  filler[nl] <-
    lapply(line_diff[nl], rep.int, x = "\n") %>%
    vapply(paste, collapse = "", character(1)) %>%
    paste0(filler[nl])

  filler
}

add_space_around_equal <- function(pd) {
  eq_sub <- pd$token == "EQ_SUB"
  eq_sub_lead <- lead(eq_sub, default = FALSE)
  stopifnot(!any(eq_sub & eq_sub_lead))
  pd$filler[eq_sub_lead] <- " "
  pd$filler[eq_sub] <- " "
  pd
}

fix_quotes <- function(pd) {
  browser()
  str_const <- pd$token == "STR_CONST"
  pd$text[str_const] <- gsub("^'([^'\"]*)'$", '"\\1"', pd$text[str_const])
  pd
}
