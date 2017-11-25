
#' @describeIn set_args Sets the argument `write_tree` in
#'   [test_collection()] to be `TRUE` for R versions higher or equal to 3.2, and
#'   `FALSE` otherwise since the second-level dependency `DiagrammeR` from
#'   `data.table` is not available for R < 3.2.
set_arg_write_tree <- function(write_tree) {
  sufficient_version <- getRversion() >= 3.2
  if (is.na(write_tree)) {
    write_tree <- ifelse(sufficient_version, TRUE, FALSE)
  } else if (!sufficient_version & write_tree) {
    stop_insufficient_r_version()
  }
  write_tree
}

set_and_assert_filetype <- function(filetype) {
  without_dot <- gsub("^\\.", "", tolower(filetype))
  assert_filetype(without_dot)
  paste0("\\.", without_dot)
}

assert_filetype <- function(lowercase_filetype) {
  if (!all(lowercase_filetype %in% c("r", "rmd"))) {
    stop(
      "filetype must not contain other values than 'R'",
      "or 'Rmd' (case is ignored).", call. = FALSE
    )
  }
}
