#' Obtain token table from text
#'
#' [utils::getParseData()] is used to obtain a flat parse table from `text`.
#'
#' Apart from the columns provided by `utils::getParseData()`, the following
#' columns are added:
#'
#'   * A column "short" with the first five characters of "text".
#'   * A column "pos_id" for (positional id) which can be used for sorting
#'     (because "id" cannot be used in general). Note that the nth value of this
#'     column corresponds to n as long as no tokens are inserted.
#'   * A column "child" that contains the nested subtibbles.
#'
#' @param text A character vector.
#' @return A flat parse table
#' @importFrom rlang seq2
tokenize <- function(text) {
  get_parse_data(text, include_text = NA) %>%
    verify_str_txt(text) %>%
    enhance_mapping_special()
}

#' Obtain robust parse data
#'
#' Wrapper around `utils::getParseData(parse(text = text))` that returns a flat
#' parse table.
#' @param text The text to parse.
#' @param include_text Passed to [utils::getParseData()] as `includeText`.
#' @param ... Other arguments passed to [utils::getParseData()].
get_parse_data <- function(text, include_text, ...) {
  # avoid https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16041
  parse(text = text, keep.source = TRUE)
  parsed <- parse(text = text, keep.source = TRUE)
  as_tibble(utils::getParseData(parsed, includeText = include_text)) %>%
    add_id_and_short()
}

#' Add column `pos_id` and `short`
#'
#' Addds column `pos_id` and `short` to a flat parse table.
#' @param pd A flat parse table
add_id_and_short <- function(pd) {
  pd$pos_id <- seq2(1L, nrow(pd))
  pd$short <- substr(pd$text, 1, 5)
  pd
}


#' Verify the text of strings
#'
#' Make sure `text` of the tokens `STR_CONST` is correct and adapt if necessary.
#' We first parse `text` again and include also non-terminal text. Then, we
#' replace offending `text` in the terminal expressions with the text of their
#' parents.
#' @param pd_with_terminal_text A parse table.
#' @param text The text from which `pd_with_terminal_text` was created. Needed
#'   for potential reparsing.
verify_str_txt <- function(pd_with_terminal_text, text) {
  string_ind <- pd_with_terminal_text$token == "STR_CONST"
  strings <- pd_with_terminal_text[string_ind,]
  parent_ind <- pd_with_terminal_text$id %in% strings$parent
  other_ind <- !(string_ind | parent_ind)
  if (nrow(strings) == 0 || !any(substr(strings$text, 1, 1) == "[")) {
    return(pd_with_terminal_text)
  }
  pd_with_all_text <- get_parse_data(text, include_text = TRUE)
  parents <- pd_with_all_text[parent_ind, c("id", "text", "short")]
  strings$text <- NULL
  strings$short <- NULL
  new_strings <- merge(strings, parents, by.x = "parent", by.y = "id")
  bind_rows(
    new_strings,
    pd_with_terminal_text[other_ind, ],
    pd_with_terminal_text[parent_ind,]
  ) %>%
    arrange(pos_id)

}
