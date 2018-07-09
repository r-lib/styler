#' Figure out where code examples start and stop
#'
#' Finds the sequence from start to stop of the lines in `text` that are
#' code examples in roxygen comments.
#' @param text A text consisting of code and/or roxygen comments.
#' @importFrom purrr map_int
#' @importFrom rlang seq2
#' @keywords internal
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

#' Match a stop candidate to a start
#' @param start An integer.
#' @param stop_candidates Potential stop candidates.
#' @examples
#' styler:::match_stop_to_start(1, c(3, 4, 5))
#' @keywords internal
match_stop_to_start <- function(start, stop_candidates) {
  min(stop_candidates[stop_candidates > start]) - 1L
}

#' Parse roxygen comments into text
#'
#' Used to parse roxygen code examples
#' @param roxygen Roxygen comments.
#' @examples
#' styler:::parse_roxygen(
#' "#' @examples
#'  #' 1+  1
#' ")
#' @keywords internal
parse_roxygen <- function(roxygen) {
  remove_roxygen_mask(roxygen) %>%
    textConnection() %>%
    tools::parse_Rd(fragment = TRUE) %>%
    as.character()
}

#' Fix parsing bugs
#'
#' @param raw Raw code to post-process.
#' @examples
#' code <- "style_text('call( 1)')
#' style_text('1    + 1', strict = FALSE)
#' style_text('a%>%b', scope = 'spaces')
#' style_text('a%>%b; a', scope = 'line_breaks')
#' style_text('a%>%b; a', scope = 'tokens')"
#' parsed <- styler:::parse_roxygen(code) # cuts before "%" for no reason
#' fixed <- styler:::post_parse_roxygen(styler:::drop_newline_codelines(parsed)) # better
post_parse_roxygen <- function(raw) {
  special <- substr(raw, 1, 1) == "%"
  len <- nchar(raw)
  newline_after <- substr(raw, len, len) == "\n"
  must_instert_linebreak_after <- which(
    (special & !newline_after) |
    (raw == "}" & (!(lead(substr(raw, 1, 1)) %in% c(",", "}", ")"))))
  )
  append_ <- purrr::partial(append, x = raw, values = "\n")
  split <- reduce(must_instert_linebreak_after +
    seq(0, length(must_instert_linebreak_after) - 1L),
    append, values = "\n", .init = raw
  ) %>%
    paste0(collapse = "") %>%
    strsplit("\n")
  split[[1]]
}

#' Style a roxygen code example that may contain a dontrun and friends
#'
#' Parses roxygen2 comments into code, breaks it into dont* (dontrun, dontest,
#' dontshow) and run sections and processes each segment indicidually using
#' [style_roxygen_dont_code_examples_one()].
#' @inheritParams parse_transform_serialize_r
#' @param example Roxygen example code.
#' @importFrom purrr map2 flatten_chr
#' @importFrom rlang seq2
#' @keywords internal
style_roxygen_code_examples_one_example <- function(example, transformers) {
  bare <- parse_roxygen(example)
  one_dont <- split(bare, factor(cumsum(bare %in% dont_keywords())))
  map(one_dont, style_roxygen_code_examples_one_dont, transformers) %>%
    flatten_chr()
}

#' Style a roxygen code example that contains at most one `\\dontrun{...}` or
#' friends
#'
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
#' @keywords internal
style_roxygen_code_examples_one_dont <- function(one_dont, transformers) {
  one_dont <- drop_newline_codelines(one_dont)
  if (length(one_dont) < 1L) return(character())
  dont_seqs <- find_dont_seqs(one_dont)
  split_segments <- split_roxygen_segments(one_dont, unlist(dont_seqs))
  is_dont <-
    seq2(1L, length(split_segments$separated)) %in% split_segments$selectors

  map2(split_segments$separated, is_dont,
       style_roxygen_dont_code_examples_one,
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

#' Given a code segment is dont* or run, style it
#'
#' @param code_segment A character vector with code to style.
#' @param is_dont Whether the segment to process is a dontrun, dontshow,
#' donttest segemnt or not.
#' @inheritParams parse_transform_serialize_r
#' @keywords internal
style_roxygen_dont_code_examples_one <- function(code_segment,
                                                    transformers,
                                                    is_dont) {
  if (is_dont) {
    decomposed <-  remove_dont_mask(code_segment)
    code_segment <- decomposed$code
    mask <- decomposed$mask
  }
  code_segment <- post_parse_roxygen(code_segment) %>%
    paste0(collapse = "\n") %>%
    parse_transform_serialize_r(transformers)

  if (is_dont) {
    code_segment <- c(mask, code_segment, "}")
  }
  code_segment
}

#' Remove dont* mask
#'
#' @param roxygen Roxygen code examples that contains a dont* segment only.
#' @keywords internal
remove_dont_mask <- function(roxygen) {
  potential_pos <- c(3L, length(roxygen) - 1L)
  is_line_break_at_potential_pos <- which(roxygen[potential_pos] == "\n")
  mask <- c(
    1, 2, length(roxygen), potential_pos[is_line_break_at_potential_pos]
  ) %>% sort()
  list(
    code = roxygen[-mask], mask = paste(roxygen[1:2], collapse = "")
  )
}


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



drop_newline_codelines <- function(code) {
  code[code != "\n"]
}

remove_roxygen_mask <- function(text) {
  code_with_header <- sub(pattern = "^#'\\s*", "", text)
  remove_roxygen_header(code_with_header)
}

remove_roxygen_header <- function(text) {
  gsub("^\\s*@examples\\s*", "", text, perl = TRUE)
}

#' @importFrom purrr map_chr
add_roxygen_mask <- function(text) {
  c(
    paste0("#' @examples"),
    map_chr(text, ~paste0("#' ", .x))
  )
}


dont_keywords <- function() {
  c("\\dontrun", "\\dontshow", "\\donttest")
}
