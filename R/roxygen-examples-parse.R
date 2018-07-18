#' Parse roxygen comments into text
#'
#' Used to parse roxygen code examples
#' @param roxygen Roxygen comments.
#' @examples
#' styler:::parse_roxygen(
#' "#' @examples
#'  #' 1+  1
#' ")
#' @keywords internal
parse_roxygen <- function(roxygen) {
  remove_roxygen_mask(roxygen) %>%
    textConnection() %>%
    tools::parse_Rd(fragment = TRUE) %>%
    as.character()
}

#' Fix parsing bugs
#'
#' @param raw Raw code to post-process.
#' @examples
#' code <- "style_text('call( 1)')
#' style_text('1    + 1', strict = FALSE)
#' style_text('a%>%b', scope = 'spaces')
#' style_text('a%>%b; a', scope = 'line_breaks')
#' style_text('a%>%b; a', scope = 'tokens')"
#' parsed <- styler:::parse_roxygen(code) # cuts before "%" for no reason
#' # better
#' fixed <- styler:::post_parse_roxygen(styler:::remove_blank_lines(parsed))
#' @keywords internal
post_parse_roxygen <- function(raw) {
  special <- substr(raw, 1, 1) == "%"
  len <- nchar(raw)
  newline_after <- substr(raw, len, len) == "\n"
  # must_instert_linebreak_after <- which(
  #   (special & !newline_after) |
  #     (raw == "}" & (!(lead(substr(raw, 1, 1)) %in% c(",", "}", ")"))))
  # )
  must_instert_linebreak_after <- integer(0)
  split <- reduce(must_instert_linebreak_after +
                    seq(0L, length(must_instert_linebreak_after) - 1L),
                  append, values = "\n", .init = raw
  ) %>%
    paste0(collapse = "") %>%
    strsplit("\n")
  split[[1]]
}
