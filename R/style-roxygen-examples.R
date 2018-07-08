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
  split <- reduce(must_instert_linebreak_after + seq(0, length(must_instert_linebreak_after) - 1L), append, values = "\n", .init = raw) %>%
    paste0(collapse = "") %>%
    strsplit("\n")
  split[[1]]
}

#' Style a roxygen code example that may contain a dontrun
#'
#' Parses roxygen2 comments into code, breaks it into dontrun / run sections and
#' processes each segment indicidually using
#' [style_roxygen_dontrun_code_examples_one()].
#' @inheritParams parse_transform_serialize_r
#' @param example Roxygen example code.
#' @importFrom purrr map2 flatten_chr
#' @importFrom rlang seq2
#' @keywords internal
style_roxygen_code_examples_one_example <- function(example, transformers) {
  bare <- parse_roxygen(example)
  one_dontrun <- split(bare, factor(cumsum(bare == "\\dontrun")))
  map(one_dontrun, style_roxygen_code_examples_one_dontrun, transformers) %>%
    flatten_chr()
}

#' Style a roxygen code example that contains at most one `\\dontrun{...}`
#'
#' We drop all newline characters first because otherwise the code segment
#' passed to this function was previously parsed with [parse_roxygen()] and
#' line-breaks in and after the `\\dontrun{...}` are expressed with `"\n"`, which
#' contradicts to the definition used elsewhere in this package, where every
#' element in a vector corresponds to a line. These line-breaks don't get
#' eliminated because they move to the front of a `code_segment` and
#' `style_text("\n1")` gives `"\n1"`, i.e. trailing newlines are not
#' eliminated.
#' @param one_dontrun Bare R code containing at most one `\\dontrun{...}`.
#' @inheritParams parse_transform_serialize_r
#' @keywords internal
style_roxygen_code_examples_one_dontrun <- function(one_dontrun, transformers) {
  one_dontrun <- drop_newline_codelines(one_dontrun)
  if (length(one_dontrun) < 1L) return(character())
  dontrun_seqs <- find_dontrun_seqs(one_dontrun)
  split_segments <- split_roxygen_segments(one_dontrun, unlist(dontrun_seqs))
  is_dontrun <-
    seq2(1L, length(split_segments$separated)) %in% split_segments$selectors

  map2(split_segments$separated, is_dontrun,
       style_roxygen_dontrun_code_examples_one,
       transformers = transformers
  ) %>%
    flatten_chr() %>%
    add_roxygen_mask()

}

#' Find dontrun sequences
#'
#' Returns the indices of the lines that correspond to a `dontrun` sequence.
#' @param bare Bare code.
#' @importFrom purrr map2 map_int
#' @keywords internal
find_dontrun_seqs <- function(bare) {
  dontrun_openings <- which(bare == "\\dontrun")
  dontrun_closings <- map_int(dontrun_openings + 1L, find_dontrun_closings, bare = bare)
  map2(dontrun_openings, dontrun_closings, seq2)
}

#' Given a code segment is dontrun or run, style it
#'
#' @param code_segment A character vector with code to style.
#' @param is_dontrun Whether the segment to process is a dontrun segemnt or not.
#' @inheritParams parse_transform_serialize_r
#' @keywords internal
style_roxygen_dontrun_code_examples_one <- function(code_segment,
                                                    transformers,
                                                    is_dontrun) {
  if (is_dontrun) {
    code_segment <- remove_dontrun_mask(code_segment)
  }
  code_segment <- post_parse_roxygen(code_segment) %>%
    paste0(collapse = "\n") %>%
    parse_transform_serialize_r(transformers)

  if (is_dontrun) {
    code_segment <- c("\\dontrun{", code_segment, "}")
  }
  code_segment
}

#' Remove dontrun mask
#'
#' @param roxygen Roxygen code examples that contains a dontrun segment only.
#' @keywords internal
remove_dontrun_mask <- function(roxygen) {
  potential_pos <- c(3L, length(roxygen) - 1L)
  is_line_break_at_potential_pos <- which(roxygen[potential_pos] == "\n")
  mask <- c(
    1, 2, length(roxygen), potential_pos[is_line_break_at_potential_pos]
  ) %>% sort()
  roxygen[-mask]
}


find_dontrun_closings <- function(bare, dontrun_openings) {
  opening <- cumsum(bare == "{")
  closing <- cumsum(bare == "}")
  diff <- opening - closing
  level_dontrun <- diff[dontrun_openings]

  all_closing_level_dontrun <- which(diff == level_dontrun & lead(diff) == (level_dontrun - 1L))
  dontrun_closing <- all_closing_level_dontrun[all_closing_level_dontrun > dontrun_openings]
  dontrun_closing + 1L
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
