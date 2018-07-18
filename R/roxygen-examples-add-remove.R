#' Remove dont* mask
#'
#' @param roxygen Roxygen code examples that contains a dont* segment only.
#' @keywords internal
remove_dont_mask <- function(roxygen) {
  mask <- c(
    1L, 2L, if (roxygen[3] == "\n") 3L, last(which(roxygen == "}"))
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
