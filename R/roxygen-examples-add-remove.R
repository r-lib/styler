#' Remove dont* mask
#'
#' @param roxygen Roxygen code examples that contains a dont* segment only.
#' @keywords internal
#' @importFrom rlang seq2
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
  code_with_header <- gsub(pattern = "^#'\\s?", "", text)
  remove_roxygen_header(code_with_header)
}

#' Remove roxygen header
#'
#' Can't simply remove the element with the regex because it may happen that
#' the roxygen tag is on the same line as its contents start.
#' @examples
#' #' @examples c(1, 2)
#' @keywords internal
remove_roxygen_header <- function(text) {
  text <- gsub("^\\s*@examples\\s*", "", text, perl = TRUE)
  starts_with_blank <- text[1] == "\n"
  c(text[1][!starts_with_blank], text[-1])
}

#' @importFrom purrr map2_chr
add_roxygen_mask <- function(text) {
  space <- ifelse(text == "", "", " ")
  c(paste0("#' @examples"), map2_chr(space, text, ~ paste0("#'", .x, .y)))
}
