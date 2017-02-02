transform_files <- function(files, transformers) {
  transformer <- function(text) {
    new_text <- Reduce(
      function(text, transformer) transformer(text),
      transformers,
      init = text)
    new_text
  }

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
      message("Please review the changes carefully!")
  }
  invisible(changed)
}

tokenize <- function(text) {
  string_regex <- rex::rex(
    capture(
      quoted_string_regex('"') %or% quoted_string_regex("'"),
      name = "string"
    ))
  splits <- strsplit(text, split = string_regex, perl = TRUE)
  splits[vapply(splits, length, integer(1)) == 0] <- ""

  tidy_splits <-
    splits %>%
    lapply(enframe, "token", "text") %>%
    enframe("line", "data") %>%
    unnest_("data")

  tidy_matches <-
    rex::re_matches(text, string_regex, global = TRUE) %>%
    enframe("line", "data") %>%
    unnest_("data") %>%
    filter_(~!is.na(string)) %>%
    group_by_(~line) %>%
    mutate_(token = ~row_number()) %>%
    ungroup()

  left_join(tidy_splits, tidy_matches, by = c("line", "token"))
}

detokenize <- function(token) {
  token %>%
    mutate_(text_string = ~paste0(text, coalesce(string, ""))) %>%
    group_by_(~line) %>%
    summarise_(text_all = ~paste0(text_string, collapse = "")) %>%
    ungroup %>%
    .[["text_all"]]
}

quoted_string_regex <- function(my_quote) {
  rex::rex(
    my_quote,
    zero_or_more(
      zero_or_more(none_of(my_quote)),
      "\\", my_quote
    ),
    zero_or_more(none_of(my_quote)),
    my_quote
  )
}
