#' Style a string
#'
#' Styles a character vector. Each element of the character vector corresponds
#' to one line of code.
#' @param text A character vector with text to style.
#' @inheritParams style_pkg
#' @family stylers
#' @examples
#' style_text("call( 1)")
#' style_text("1    + 1", strict = FALSE)
#' style_text("a%>%b", scope = "spaces")
#' style_text("a%>%b; a", scope = "line_breaks")
#' style_text("a%>%b; a", scope = "tokens")
#' # the following is identical but the former is more convenient:
#' style_text("a<-3++1", style = tidyverse_style, strict = TRUE)
#' @examples
#' \dontrun{style_text("a<-3++1", transformers = tidyverse_style(strict = TRUE))}
#' @export
style_text <- function(text,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE) {
  transformer <- make_transformer(transformers, include_roxygen_examples)
  styled_text <- transformer(text)
  construct_vertical(styled_text)
}
