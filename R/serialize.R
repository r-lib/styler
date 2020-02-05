#' Serialize flattened parse data
#'
#' Collapses a flattened parse table into character vector representation.
#' @inheritParams apply_stylerignore
#' @keywords internal
serialize_parse_data_flattened <- function(flattened_pd) {
  flattened_pd <- apply_stylerignore(flattened_pd)
  flattened_pd$lag_newlines[1] <- 0 # resolve start_line elsewhere
  res <- with(
    flattened_pd,
    paste0(
      collapse = "",
      map(lag_newlines, add_newlines), map(lag_spaces, add_spaces), text
    )
  )
  convert_newlines_to_linebreaks(res)
}
