#' Figure out where code examples start and stop
#'
#' Finds the sequence from start to stop of the lines in `text` that are
#' code examples in roxygen comments.
#' @param text A text consisting of code and/or roxygen comments.
#' @importFrom purrr map_int map2
#' @importFrom rlang seq2
#' @keywords internal
identify_start_to_stop_of_roxygen_examples_from_text <- function(text) {
  starts <- grep("^#'\\s*@examples", text, perl = TRUE)
  stop_candidates <- grep("^[^#]|^#'\\s*@", text, perl = TRUE)
  stops <- map_int(starts, match_stop_to_start, stop_candidates)
  map2(starts, stops, seq2)
}

identify_start_to_stop_of_roxygen_examples <- function(path) {
  content <- enc::read_lines_enc(path)
  identify_start_to_stop_of_roxygen_examples_from_text(content)
}

#' Match a stop candidate to a start
#' @param start An integer.
#' @param stop_candidates Potential stop candidates.
#' @examples
#' styler:::match_stop_to_start(1, c(3, 4, 5))
#' @keywords internal
match_stop_to_start <- function(start, stop_candidates) {
  min(stop_candidates[stop_candidates > start]) - 1L
}

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
    flatten_chr()
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
  one_dont <- remove_blank_lines(one_dont)
  if (length(one_dont) < 1L) return(character())
  dont_seqs <- find_dont_seqs(one_dont)
  split_segments <- split_roxygen_segments(one_dont, unlist(dont_seqs))
  is_dont <-
    seq2(1L, length(split_segments$separated)) %in% split_segments$selectors

  map2(split_segments$separated, is_dont,
       style_roxygen_example_snippet,
    transformers = transformers
  ) %>%
    flatten_chr() %>%
    add_roxygen_mask()

}

#' Find dontrun and friend sequences
#'
#' Returns the indices of the lines that correspond to a `dontrun` or
#' friends sequence.
#' @param bare Bare code.
#' @importFrom purrr map2 map_int
#' @keywords internal
find_dont_seqs <- function(bare) {
  dont_openings <- which(bare %in% dont_keywords())
  dont_type <- bare[dont_openings]
  dont_closings <- map_int(dont_openings + 1L, find_dont_closings, bare = bare)
  map2(dont_openings, dont_closings, seq2)
}

#' Given a code snippet is dont* or run, style it
#'
#' @param code_snippet A character vector with code to style.
#' @param is_dont Whether the snippet to process is a dontrun, dontshow,
#'   donttest segemnt or not.
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
    parse_transform_serialize_r(transformers)

  if (is_dont) {
    code_snippet <- c(mask, code_snippet, "}")
  }
  code_snippet
}

#' @importFrom rlang seq2
find_dont_closings <- function(bare, dont_openings) {
  opening <- cumsum(bare == "{")
  closing <- cumsum(bare == "}")
  diff <- opening - closing
  level_dont <- diff[dont_openings]
  match_closing <- intersect(
    seq2(dont_openings + 1L, length(bare)),
    which(diff == level_dont - 1L)
  )[1]
  match_closing
}

dont_keywords <- function() {
  c("\\dontrun", "\\dontshow", "\\donttest")
}
