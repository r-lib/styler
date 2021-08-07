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
  gsub("^[\\s\t]*@examples(If)?(\\s|\t)*", "", text, perl = TRUE)
}

#' Add the roxygen mask to code
#'
#' This function compares `text` with `initial_text` to make sure a mask is only
#' added to roxygen comments, not ordinary comments
#' @param text Character vector with code.
#' @param initial_text The roxygen code example to style with mask and
#'   potentially ordinary comments.
#' @param example_type Either 'examples' or 'examplesIf'.
#' @keywords internal
#' @importFrom purrr map2_chr
add_roxygen_mask <- function(text, initial_text, example_type) {
  space <- ifelse(text == "", "", " ")
  out <- c(
    paste0("#' @", example_type, space[1], text[1]),
    map2_chr(space[-1], text[-1], ~ paste0("#'", .x, .y))
  )

  ordinary_comment <- grep("^#[^']", initial_text, value = TRUE)
  if (length(ordinary_comment) == 0L) {
    return(out)
  }
  without_mask <- remove_roxygen_mask(out)
  for (idx in seq_along(ordinary_comment)) {
    to_replace <- which(ordinary_comment[idx] == without_mask)[1]
    out[to_replace] <- ordinary_comment[idx]
    without_mask[to_replace] <- NA
  }
  out
}
