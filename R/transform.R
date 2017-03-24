transform_files <- function(files, transformers) {
  transformer <- make_transformer(transformers)

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
    message("Please review the changes carefully!")
  }
  invisible(changed)
}

#' Closure to return a transformer function
#'
#' This function takes a list of transformer functions as input and
#'  returns a function that can be applied to character strings
#'  that should be transformed.
#' @param transformers A list of transformer functions.
make_transformer <- function(transformers) {
  function(text) {
    text <- gsub(" +$", "", text)
    text <- gsub("\t", "        ", text)

    parsed <- parse(text = text, keep.source = TRUE)
    parse_data <- tbl_df(utils::getParseData(parsed, includeText = TRUE))
    parse_data_nested <-
      parse_data %>%
      mutate_(short = ~substr(text, 1, 5)) %>%
      select_(~short, ~everything()) %>%
      nest_parse_data

    browser()
    #' TODO:
    #' - Walk tree defined by `leaves`, compute whitespace
    #' - Perform all transformations on hierarchical structure
    #'     - Compute text for a sub-element
    #' - Compute indentation
    #'     - Braces
    #'     - Function calls
    #'     - Function definitions
    #' - Remove `includeText = TRUE`
    parse_data_with_ws <- add_ws_to_parse_data(parse_data)

    # May strip empty lines before EOF
    text <- verify_roundtrip(parse_data_with_ws, text)

    transformed_data_with_ws <- Reduce(
      function(x, fun) fun(x),
      transformers,
      init = parse_data_with_ws)

    new_text <- serialize_parse_data(transformed_data_with_ws)
    new_text
  }
}
