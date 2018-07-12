#' Remove dont* mask
#'
#' @param roxygen Roxygen code examples that contains a dont* segment only.
#' @keywords internal
remove_dont_mask <- function(roxygen) {
  potential_pos <- c(3L, length(roxygen) - 1L)
  is_line_break_at_potential_pos <- which(roxygen[potential_pos] == "\n")
  mask <- c(
    1L, 2L, length(roxygen), potential_pos[is_line_break_at_potential_pos]
  ) %>% sort()
  list(
    code = roxygen[-mask], mask = paste(roxygen[seq2(1, 2)], collapse = "")
  )
}

remove_blank_lines <- function(code) {
  code[code != "\n"]
}

remove_roxygen_mask <- function(text) {
  code_with_header <- sub(pattern = "^#'\\s*", "", text)
  remove_roxygen_header(code_with_header)
}

remove_roxygen_header <- function(text) {
  sub("^\\s*@examples\\s*", "", text, perl = TRUE)
}

#' @importFrom purrr map_chr
add_roxygen_mask <- function(text) {
  c(paste0("#' @examples"), map_chr(text, ~paste0("#' ", .x)))
}
