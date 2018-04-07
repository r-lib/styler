#' Figure out where code examples start and stop
#'
#' Finds the sequence from start to stop of the lines in `text` that are
#' code examples in roxygen comments.
#' @param text A text consisting of code and/or roxygen comments.
#' @importFrom purrr map_int
#' @importFrom rlang seq2
identify_start_to_stop_of_roxygen_examples_from_text <- function(text) {
  starts <- grep("^#'\\s*@examples", text, perl = TRUE)
  stop_candidates <- grep("^[^#]|^#'\\s*@", text, perl = TRUE)
  stops <- map_int(starts, match_stop_to_start, stop_candidates)
  map2(starts, stops, seq2)
}

identify_start_to_stop_of_roxygen_examples <- function(path) {
  content <- enc::read_lines_enc(path) # ensure file can be read
  identify_start_to_stop_of_roxygen_examples_from_text(content)
}

match_stop_to_start <- function(start, stop_candidates) {
  min(stop_candidates[stop_candidates > start]) - 1L
}

style_roxygen_code_examples_one <- function(example, transformers) {
  bare <- remove_roxygen_mask(example)
  styled <- parse_transform_serialize_r(bare, transformers)
  masked <- add_roxygen_mask(styled)
  masked
}


remove_roxygen_mask <- function(text) {
  code_with_header <- sub(pattern = "^#'\\s*", "", text)
  remove_roxygen_header(code_with_header)
}

remove_roxygen_header <- function(text) {
  gsub("^\\s*@examples\\s*", "", text, perl = TRUE)
}

add_roxygen_mask <- function(text) {
  c(
    trimws(paste0("#' @examples ", text[1])),
    map_chr(text[-1], ~paste0("#' ", .x))
  )
}
