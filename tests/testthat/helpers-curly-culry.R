style_text_without_curly_curly <- function(text,
                                           ...,
                                           style = tidyverse_style,
                                           transformers = style(...),
                                           include_roxygen_examples = TRUE) {
  dots <- list(...)
  if ("strict" %in% names(dots)) {
    strict <- dots$strict
  } else {
    strict <- TRUE
  }
  transformers$line_break$style_line_break_around_curly <-
    purrr::partial(style_line_break_around_curly,
      curly_curly_has_linebreak = TRUE, strict = strict
    )
  style_text(text, ...,
    style = NULL, transformers = transformers,
    include_roxygen_examples = include_roxygen_examples
  )
}
