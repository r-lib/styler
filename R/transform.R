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

    parse_data_with_ws <- compute_parse_data_flat_with_ws(text)

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
