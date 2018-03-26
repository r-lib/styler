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
#'   * A column "child" that contains *nest*s.
#'
#' @param text A character vector.
#' @return A flat parse table
#' @importFrom rlang seq2
#' @keywords internal
tokenize <- function(text) {
  get_parse_data(text, include_text = NA) %>%
    ensure_correct_str_txt(text) %>%
    enhance_mapping_special()
}

#' Obtain robust parse data
#'
#' Wrapper around `utils::getParseData(parse(text = text))` that returns a flat
#' parse table.
#' @param text The text to parse.
#' @param include_text Passed to [utils::getParseData()] as `includeText`.
#' @param ... Other arguments passed to [utils::getParseData()].
#' @keywords internal
get_parse_data <- function(text, include_text = TRUE, ...) {
  # avoid https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16041
  parse(text = text, keep.source = TRUE)
  parsed <- parse(text = text, keep.source = TRUE)
  as_tibble(utils::getParseData(parsed, includeText = include_text)) %>%
    add_id_and_short()
}

#' Add column `pos_id` and `short`
#'
#' Adds column `pos_id` and `short` to a flat parse table.
#' @param pd A flat parse table
#' @keywords internal
add_id_and_short <- function(pd) {
  pd$pos_id <- seq2(1L, nrow(pd))
  pd$short <- substr(pd$text, 1, 5)
  pd
}


#' Ensure a correct `text` of all strings
#'
#' Make sure `text` of the tokens `STR_CONST` is correct and adapt if necessary.
#' We first parse `text` again and include also non-terminal text. Then, we
#' replace offending `text` in the terminal expressions with the text of their
#' parents if their line / col position matches and return an error otherwise.
#' @param pd A parse table.
#' @param text The text from which `pd` was created. Needed
#'   for potential reparsing.
#' @keywords internal
ensure_correct_str_txt <- function(pd, text) {
  is_problematic_string <- identify_insufficiently_parsed_stings(pd, text)
  problematic_strings <- pd[is_problematic_string, ]
  is_parent_of_problematic_string <-
    pd$id %in% problematic_strings$parent

  is_unaffected_token <- !(is_problematic_string | is_parent_of_problematic_string)
  if (!any(is_problematic_string)) {
    return(pd)
  }

  pd_with_all_text <- get_parse_data(text, include_text = TRUE)
  parent_cols_for_merge <- c("id", "text", "short", line_col_names())
  parent_of_problematic_strings <-
    pd_with_all_text[is_parent_of_problematic_string, parent_cols_for_merge]
  problematic_strings$text <- NULL
  problematic_strings$short <- NULL
  new_strings <- merge(problematic_strings, parent_of_problematic_strings,
    by.x = "parent", by.y = "id"
  ) %>%
    as_tibble()

  if (!lines_and_cols_match(new_strings)) {
    stop(paste(
      "Error in styler:::ensure_correct_str_txt().",
      "Please file an issue on GitHub (https://github.com/r-lib/styler/issues)",
    ), call. = FALSE)
  }
  bind_rows(
    new_strings,
    pd[is_unaffected_token, ],
    pd[is_parent_of_problematic_string, ]
  ) %>%
    arrange(pos_id)
}

#' Indentify strings that were not fully parsed
#'
#' Indentifies strings that were not fully parsed due to their vast length.
#' @details
#' The meaning of the variable `is_problematic_string` in the source code
#' changes from "all strings" to "all problematic strings", is partly
#' missleading and this approach was choosen for performance reasons only.
#' @param pd A parse table.
#' @param text The initial code to style.
identify_insufficiently_parsed_stings <- function(pd, text) {
  is_problematic_string <- pd$token == "STR_CONST"
  candidate_substring <- substr(
    pd$text[is_problematic_string], 1, 1
  )
  is_problematic_string[is_problematic_string] <- candidate_substring == "["
  is_problematic_string
}

#' @importFrom purrr map2_lgl
lines_and_cols_match <- function(data) {
  left <- paste0(line_col_names(), ".x")
  right <- paste0(line_col_names(), ".y")
  map2_lgl(left, right,
    two_cols_match,
    data = data
  ) %>%
  all()
}
