parse_text <- function(x) parse_safely(x)[[1L]]

line_col_names <- function() {
  c("line1", "line2", "col1", "col2")
}

#' Wrapper functions to encapsulate data frame creation
#' @keywords internal
#' @noRd
styler_df <- function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}

#' @keywords internal
#' @noRd
new_styler_df <- function(x) {
  vctrs::new_data_frame(x)
}

#' Ensure there is one (and only one) blank line at the end of a vector
#' @examples
#' styler:::ensure_last_n_empty("")
#' styler:::ensure_last_n_empty(letters)
#' styler:::ensure_last_n_empty(c(letters, "", "", ""))
#' @keywords internal
ensure_last_n_empty <- function(x, n = 1L) {
  if (all(x == "")) {
    return("")
  }
  x <- c(x, "", "")
  x <- x[seq(1L, length(x) - which(rev(x) != "")[1L] + 1L)]
  c(x, rep("", n))
}

#' @note Slightly simplified version of `rematch2::re_match()` (License: MIT).
#' @keywords internal
#' @noRd
re_match <- function(text, pattern) {
  stopifnot(is.character(pattern), length(pattern) == 1L, !is.na(pattern))
  text <- as.character(text)
  match <- regexpr(pattern, text, perl = TRUE)
  start <- as.vector(match)
  length <- attr(match, "match.length")
  end <- start + length - 1L
  matchstr <- substring(text, start, end)
  matchstr[start == -1L] <- NA_character_
  res <- data.frame(stringsAsFactors = FALSE, .text = text, .match = matchstr)

  gstart <- attr(match, "capture.start")
  glength <- attr(match, "capture.length")
  gend <- gstart + glength - 1L
  groupstr <- substring(text, gstart, gend)
  groupstr[gstart == -1L] <- NA_character_
  dim(groupstr) <- dim(gstart)
  res <- cbind(groupstr, res, stringsAsFactors = FALSE)

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}

#' Replace the newline character with a line break
#'
#' @param text A character vector
#' @examples
#' styler:::convert_newlines_to_linebreaks("x\n2")
#' # a simple strsplit approach does not cover both cases
#' unlist(strsplit("x\n\n2", "\n", fixed = TRUE))
#' unlist(strsplit(c("x", "", "2"), "\n", fixed = TRUE))
#' styler:::convert_newlines_to_linebreaks(c("x", "2"))
#' @keywords internal
convert_newlines_to_linebreaks <- function(text) {
  split <- strsplit(text, "\n", fixed = TRUE)
  map(split, ~ if (identical(.x, character(0L))) {
    ""
  } else {
    .x
  }) %>%
    unlist(use.names = FALSE)
}

odd_index <- function(x) {
  if (length(x) < 1L) {
    return(NULL)
  }
  seq(1L, length(x), by = 2L)
}

even_index <- function(x) {
  seq(2L, length(x), by = 2L)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Invoke a system command
#'
#' Wraps a system command into [shell()] or [system()], depending on the
#' operating system.
#' @param sys_call The call to be executed.
#' @param ... Arguments passed to [shell()] or [system()].
#' @keywords internal
calls_sys <- function(sys_call, ...) {
  if (is_windows()) {
    error <- shell(sys_call, ...)
  } else {
    error <- system(sys_call, ...)
  }
  error
}

#' Get the value of an option
#'
#' Basically a `getOptions()` that fails fast by default.
#' @inheritParams base::getOption
#' @param error_if_not_found Whether or not an error should be returned if the
#'   option was not set.
#' @keywords internal
option_read <- function(x, default = NULL, error_if_not_found = TRUE) {
  if (x %in% names(options()) || !error_if_not_found) {
    getOption(x, default)
  } else {
    rlang::abort(paste("R option", x, "must be set."))
  }
}

#' @keywords internal
unwhich <- function(x, length) {
  x_ <- rep(FALSE, length)
  x_[x] <- TRUE
  x_
}
