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
  stops <- map(starts, match_stop_to_start, stop_candidates) %>%
    flatten_int()
  if (length(stops) < 1L) {
    return(integer())
  }

  map2(starts, stops, seq2)
}

identify_start_to_stop_of_roxygen_examples <- function(path) {
  content <- xfun::read_utf8(path)
  identify_start_to_stop_of_roxygen_examples_from_text(content)
}

#' Match a stop candidate to a start
#' @param start An integer.
#' @param stop_candidates Potential stop candidates.
#' @examples
#' styler:::match_stop_to_start(1, c(3, 4, 5))
#' @keywords internal
match_stop_to_start <- function(start, stop_candidates) {
  is_stop_candidate <- stop_candidates > start
  if (any(is_stop_candidate)) {
    min(stop_candidates[is_stop_candidate]) - 1L
  } else {
    integer()
  }
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
  match_closing + 1L
}
