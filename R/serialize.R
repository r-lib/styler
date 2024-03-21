#' Serialize flattened parse data
#'
#' Collapses a flattened parse table into character vector representation.
#' @inheritParams apply_stylerignore
#' @param indent_character The character that is used for indention. We strongly
#'   advise for using spaces as indention characters.
#' @keywords internal
serialize_parse_data_flattened <- function(flattened_pd, indent_character = "") {
  flattened_pd <- apply_stylerignore(flattened_pd)
  flattened_pd$lag_newlines[1L] <- 0L # resolve start_line elsewhere
  with(
    flattened_pd,
    paste0(
      collapse = "",
      map(lag_newlines, add_newlines),
      map2(
        ifelse(lag_newlines > 0L, indent_character, " "),
        lag_spaces,
        rep_char
      ),
      text
    )
  )
}
