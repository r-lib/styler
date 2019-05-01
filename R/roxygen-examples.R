#' Style a roxygen code example that may contain dontrun and friends
#'
#' Parses roxygen2 comments into code, breaks it into dont* (dontrun, dontest,
#' dontshow) and run sections and processes each segment indicidually using
#' [style_roxygen_example_snippet()].
#' @inheritParams parse_transform_serialize_r
#' @param example Roxygen example code.
#' @inheritSection parse_transform_serialize_roxygen Hierarchy
#' @importFrom purrr map flatten_chr
#' @keywords internal
style_roxygen_code_example <- function(example, transformers) {
  bare <- parse_roxygen(example)
  one_dont <- split(bare, factor(cumsum(bare %in% dont_keywords())))
  map(one_dont, style_roxygen_code_example_segment, transformers) %>%
    flatten_chr() %>%
    add_roxygen_mask()
}

#' Style a roxygen code example segment
#'
#' A roxygen code example segment corresponds to roxygen example code that
#' contains at most one `\\dontrun{...}` or friends.
#' We drop all newline characters first because otherwise the code segment
#' passed to this function was previously parsed with [parse_roxygen()] and
#' line-breaks in and after the `\\dontrun{...}` are expressed with `"\n"`, which
#' contradicts to the definition used elsewhere in this package, where every
#' element in a vector corresponds to a line. These line-breaks don't get
#' eliminated because they move to the front of a `code_segment` and
#' `style_text("\n1")` gives `"\n1"`, i.e. trailing newlines are not
#' eliminated.
#' @param one_dont Bare R code containing at most one `\\dontrun{...}` or
#'   friends.
#' @inheritParams parse_transform_serialize_r
#' @inheritSection parse_transform_serialize_roxygen Hierarchy
#' @importFrom rlang seq2
#' @importFrom purrr map2 flatten_chr
#' @keywords internal
style_roxygen_code_example_segment <- function(one_dont, transformers) {
  if (length(one_dont) < 1L) {
    return(character())
  }
  dont_seqs <- find_dont_seqs(one_dont)
  split_segments <- split_roxygen_segments(one_dont, unlist(dont_seqs))
  is_dont <-
    seq2(1L, length(split_segments$separated)) %in% split_segments$selectors

  map2(split_segments$separated, is_dont,
    style_roxygen_example_snippet,
    transformers = transformers
  ) %>%
    flatten_chr()
}

#' Given a code snippet is dont* or run, style it
#'
#' @param code_snippet A character vector with code to style.
#' @param is_dont Whether the snippet to process is a dontrun, dontshow,
#'   donttest segment or not.
#' @inheritParams parse_transform_serialize_r
#' @inheritSection parse_transform_serialize_roxygen Hierarchy
#' @keywords internal
style_roxygen_example_snippet <- function(code_snippet,
                                          transformers,
                                          is_dont) {
  if (is_dont) {
    decomposed <- remove_dont_mask(code_snippet)
    code_snippet <- decomposed$code
    mask <- decomposed$mask
  }
  code_snippet <- post_parse_roxygen(code_snippet) %>%
    paste0(collapse = "\n") %>%
    parse_transform_serialize_r(transformers, warn_empty = FALSE)

  if (is_dont) {
    code_snippet <- c(mask, code_snippet, "}")
  }
  code_snippet
}

dont_keywords <- function() {
  c("\\dontrun", "\\dontshow", "\\donttest")
}
