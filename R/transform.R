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
  quotes <- c('"', "'", "`", "%")
  quoted_regex <-
    Reduce(rex:::or, lapply(quotes, quoted_string_regex))

  string_regex <- rex::rex(capture(quoted_regex, name = "string"))

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

broken_comment_tokenizer <- function() {
  comment_regex <-
    rex::rex(
      start,
      capture(
        zero_or_more(none_of(c(quotes, "#"))),
        zero_or_more(
          escape("(?:"),
          quoted_regex,
          escape(")"),
          zero_or_more(none_of(c(quotes, "#")))
        ),
        name = "code"
      ),
      zero_or_one(
        capture(
          "#",
          zero_or_more(any),
          name = "comment"
        )
      ),
      end
    )

  tidy_code_comments <-
    rex::re_matches(text, comment_regex) %>%
    mutate_(line = ~seq_along(code))
}

use_empty_strings <- function(token) {
  string_regex <- rex::rex(
    start,
    capture(any),
    zero_or_more(any),
    capture(any),
    end
  )

  token %>%
    mutate_(string = ~rex::re_substitutes(string, string_regex, "\\1\\2"))
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
