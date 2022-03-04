parse_text <- function(x) parse_safely(x)[[1L]]

line_col_names <- function() {
  c("line1", "line2", "col1", "col2")
}

#' Ensure there is one (and only one) blank line at the end of a vector
#' @examples
#' styler:::ensure_last_n_empty("")
#' styler:::ensure_last_n_empty(letters)
#' styler:::ensure_last_n_empty(c(letters, "", "", ""))
#' @keywords internal
ensure_last_n_empty <- function(x, n = 1) {
  if (all(x == "")) {
    return("")
  }
  x <- c(x, "", "")
  x <- x[seq(1, length(x) - which(rev(x) != "")[1] + 1L)]
  c(x, rep("", n))
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
  map(split, ~ if (identical(.x, character(0))) {
    ""
  } else {
    .x
  }) %>%
    unlist()
}

#' Check whether two columns match
#'
#' @param col1,col2 Column names as string.
#' @param data The data frames that contains `col1` and `col2`.
#' @keywords internal
two_cols_match <- function(col1, col2, data) {
  all(unlist(data[col1]) == unlist(data[col2]))
}

odd <- function(x) {
  x[odd_index(x)]
}

odd_index <- function(x) {
  if (length(x) < 1) {
    return(NULL)
  }
  seq(1L, length(x), by = 2)
}

even <- function(x) {
  if (length(x) < 2) {
    return(NULL)
  }
  x[even_index(x)]
}

even_index <- function(x) {
  seq(2L, length(x), by = 2)
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
}

#' Get the value of an option
#'
#' Basically a `getOptions()` that fails fast by default.
#' @inheritParams base::getOption
#' @param error_if_not_found Whether or not an error should be returned if the
#'   option was not set.
#' @keywords internal
option_read <- function(x, default = NULL, error_if_not_found = TRUE) {
  if (x %in% names(options()) | !error_if_not_found) {
    getOption(x, default)
  } else {
    rlang::abort(paste("R option", x, "must be set."))
  }
}


unwhich <- function(x, length) {
  x_ <- rep(FALSE, length)
  x_[x] <- TRUE
  x_
}
