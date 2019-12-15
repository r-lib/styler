#' Serialize flattened parse data
#'
#' Collapses a flattened parse table into character vector representation.
#' @inheritParams apply_stylerignore
#' @param start_line The line number on which the code starts.
#' @keywords internal
serialize_parse_data_flattened <- function(flattened_pd, start_line = 1) {
  flattened_pd$lag_newlines[1] <- start_line - 1
  flattened_pd <- apply_stylerignore(flattened_pd)
  res <- with(
    flattened_pd,
    paste0(
      collapse = "",
      map(lag_newlines, add_newlines), map(lag_spaces, add_spaces), text
    )
  )
  convert_newlines_to_linebreaks(res)
}
