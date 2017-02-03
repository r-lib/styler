transform_files <- function(files, transformers) {
  transformer <- make_transformer(transformers)

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
    message("Please review the changes carefully!")
  }
  invisible(changed)
}

make_transformer <- function(transformers) {
  function(text) {
    text <- gsub(" +$", "", text)
    text <- gsub("\t", "        ", text)

    parsed <- parse(text = text, keep.source = TRUE)
    parse_data <- utils::getParseData(parsed)
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
