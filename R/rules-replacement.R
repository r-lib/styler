force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}


resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) {
    return(pd)
  }
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}


#' Replace single quotes with double quotes
#'
#' @details
#' Depending on whether the backslash represents a character on its own or
#' whether it is used to create a special character, we either use:
#'
#' - `deparse(parse_text(...))`
#' - `parse(paste("\"", ., "\""))`
#'
#' To make sure esaping is done correctly.
#' @examples
#' charToRaw("\n") # one character
#' charToRaw("\\n") # two characters
#' charToRaw("\\\n") # two characters
#' # put these into a file and style with style_file(),
#' # style_text() escapes backslash.
#' 'here
#' is a string
#' '
#' 'here\nis a string
#' '
#' 'here\\nis a string'
#' @importFrom purrr map map_chr
#' @param pd_flat A flat parse table.
#' @keywords internal
fix_quotes <- function(pd_flat) {
  str_const <- pd_flat$token == "STR_CONST"
  str_const_change <- grepl("^'([^\"]*)'$", pd_flat$text[str_const])
  pd_flat$text[str_const][str_const_change] <- map_chr(
    pd_flat$text[str_const][str_const_change], fix_quotes_one
  )
  pd_flat
}

fix_quotes_one <- function(x) {
  one <- has_one_backslash(x)
  multiple <- has_multiple_backslashes(x)

  if (one && multiple) {
    x
  } else if (one) {
    x %>%
      parse_text() %>%
      paste0("\"", ., "\"")
  } else {
    parse_text(x) %>%
      deparse()
  }
}
