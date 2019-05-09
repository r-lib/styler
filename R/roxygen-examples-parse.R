#' Parse roxygen comments into text
#'
#' Used to parse roxygen code examples. Removes line break before
#' `\\dontrun{...}` and friends because it does not occurr for segments other
#' than `\\dont{...}` and friends.
#' @param roxygen Roxygen comments.
#' @examples
#' styler:::parse_roxygen(c(
#'   "#' @examples",
#'   "#' 1+  1"
#' ))
#' styler:::parse_roxygen(c(
#'   "#' @examples 33",
#'   "#'1+  1"
#' ))
#' @keywords internal
parse_roxygen <- function(roxygen) {
  connection <- remove_roxygen_mask(roxygen) %>%
    textConnection()
  parsed <- connection %>%
    tools::parse_Rd(fragment = TRUE) %>%
    as.character(deparse = TRUE)
  is_line_break <- parsed[1] == "\n"
  close(connection)
  c(parsed[1][!is_line_break], parsed[-1])
}

#' Changing the line definition
#'
#' Input: New line denoted with `\\n`. Lines can span across elements.
#' Output: Each element in the vector is one line.
#'
#' @param raw Raw code to post-process.
#' @keywords internal
post_parse_roxygen <- function(raw) {
  split <- raw %>%
    paste0(collapse = "") %>%
    strsplit("\n", fixed = TRUE)
  split[[1]]
}
