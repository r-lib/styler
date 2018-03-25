#' Figure out where code examples start and stop
#'
#' Finds the start and stop indices of the lines in `text` that are
#' code examples in roxygen comments.
#' @param text
identify_start_stop_of_roxygen_examples_from_text <- function(text) {
  starts <- grep("^#'\\s@examples", text, perl = TRUE)
  stop_candidates <- grep("^[^#]|^#'\\s@", text, perl = TRUE)
  stops <- sapply(starts, match_stop_to_start, stop_candidates)
  map2(starts, stops, c)
}

identify_start_stop_of_roxygen_examples <- function(path) {
  content <- enc::read_lines_enc(path) # ensure file can be read
  identify_start_stop_of_roxygen_examples_from_text(content)
}

match_stop_to_start <- function(start, stop_candidates) {
  min(stop_candidates[stop_candidates > start]) - 1
}

#' TODO:
#' * move to R/ui.R
#' * same arguments as other stylers
#' * add include_roxygen_code_examples argument to style_pkg() et al.
#' * export
style_roxygen_code_examples <- function(path) {
  map(path, style_roxygen_code_examples_one)
}

style_roxygen_code_examples_one <- function(path) {
  full_file_content <- enc::read_lines_enc(path)
  start_stop_sequences <- identify_start_and_stop_of_royxgen_examples_from_paths(
    full_file_content
  ) %>%
    start_stop_pairs_to_sequences()

  masked_examples <- extract_selected_lines_from_text(
    full_file_content, start_stop_sequences
  )

  plain_examples <- map(start_stop_sequences, remove_roxygen_mask,
    text = masked_examples
  )
  styled_examples <- map(plain_examples, style_text, ...)
  masked_examples <- map(styled_examples, add_roxygen_mask)
  full_file_content <- update_selected_lines_of_text(
    full_file_content, masked_examples, start_stop_sequences
  )
  enc::write_lines_enc(path, full_file_content)
}


remove_roxygen_mask <- function(text) {
  sub(pattern = "#'(\\s)?", "", text)
}

add_roxygen_mask <- function(text) {
  sapply(text, FUN = function(x) paste0("#' ", x))
}
