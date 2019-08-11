#' Serialize flattened parse data
#'
#' Collapses a flattened parse table into character vector representation.
#' @param flattened_pd A flattened parse table.
#' @param start_line The line number on which the code starts.
#' @keywords internal
serialize_parse_data_flattened <- function(flattened_pd, start_line = 1) {
  flattened_pd$lag_newlines[1] <- start_line - 1
  res <- with(
    flattened_pd,
    paste0(
      collapse = "",
      map(lag_newlines, add_newlines), map(lag_spaces, add_spaces), text
    )
  )
  strsplit(res, "\n")[[1L]]
}
