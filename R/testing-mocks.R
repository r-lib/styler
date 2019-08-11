#' `style_text()` without rules for `\{\{`
#'
#' This function mocks [style_text()], but without taking into consideration the
#' rules for the curly-curly syntactic sugar (introduced in rlang 0.4).
#' This function (`style_text_without_curly_curly()`) is needed for testing
#' only, namely to test indention
#' with multiple curly braces in a sequence. It is important to maintain testing
#' for indention rules even as the curly-curly expression is always kept on the
#' same line in the tidyverse style guide because we should
#' ensure the underlaying mechanics for indention work correctly. When
#' indention mechanisms are changed later, e.g. by simplifying
#' [compute_indent_indices()], we must have
#' a way of testing this without the interaction of `\{\{`.
#' @examples
#' styler:::style_text_without_curly_curly("rlang::list2({{ x }} := 2)")
#' styler:::style_text("rlang::list2({{ x }} := 3)")
#' @keywords internal
#' @seealso set_line_break_around_curly_curly
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
  transformers$line_break$set_line_break_around_curly_curly <- NULL
  style_text(text, ...,
    style = NULL, transformers = transformers,
    include_roxygen_examples = include_roxygen_examples
  )
}
