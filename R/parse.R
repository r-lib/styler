#' Save parsing from text
#'
#' Parses text safely, i.e. throws an informative error if EOL style does not
#' match LF or indicates the exact position where the parsing failed. Note
#' that we can only detect wrong EOL style if it occurs on the first line
#' already.
#' @param text Text to parse.
#' @param ... Parameters passed to [base::parse()].
#' @keywords internal
#' @examples
#' try(styler:::parse_safely("a + 3 -4 -> x\r\n glück + 1"))
#' # This cannot be detected as a EOL style problem because the first
#' # line ends as expected with \n
#' try(styler:::parse_safely("a + 3 -4 -> x\nx + 2\r\n glück + 1"))
#'
#' styler:::parse_safely("a + 3 -4 -> \n glück + 1")
parse_safely <- function(text, ...) {
  tried_parsing <- rlang::with_handlers(
    parse(text = text, ...),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(tried_parsing, "error")) {
    if (has_crlf_as_first_line_sep(tried_parsing$message, text)) {
      abort(paste0(
        "The code to style seems to use Windows style line endings (CRLF). ",
        "styler currently only supports Unix style line endings (LF). ",
        "Please change the EOL character in your editor to Unix style and try ",
        "again.\nThe parsing error was:\n", tried_parsing$message
      ))
    } else {
      abort(tried_parsing$message)
    }
  } else if (inherits(tried_parsing, "warning")) {
    warn(tried_parsing$message)
  }
  tried_parsing
}

#' Check if a string uses CRLF EOLs
#'
#' @param message A message returned with `tryCatch()`.
#' @param initial_text The initial text to style.
#' @keywords internal
has_crlf_as_first_line_sep <- function(message, initial_text) {
  split <- strsplit(message, ":", fixed = TRUE)[[1L]]
  if (length(split) > 1L && split[1L] == "<text>") {
    start_char <- as.numeric(split[3L])
    offending_line <- initial_text[as.integer(split[2L])]
    if (!is.na(offending_line) && substr(offending_line, start_char, start_char + 1L) == "\r\n") {
      return(TRUE)
    }
  }
  FALSE
}
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
#' @inheritParams get_parse_data
#' @return A flat parse table
#'
#' @keywords internal
tokenize <- function(text) {
  get_parse_data(text, include_text = TRUE) %>%
    ensure_correct_txt(text) %>%
    enhance_mapping_special()
}

#' Obtain robust parse data
#'
#' Wrapper around `utils::getParseData(parse(text = text))` that returns a flat
#' parse table. When caching information should be added, make sure that
#' the cache is activated with `cache_activate()` and both `transformers` and
#' `cache_dir` are non-`NULL`.
#' @param text The text to parse.
#' @param include_text Passed to [utils::getParseData()] as `includeText`.
#' @param ... Other arguments passed to [utils::getParseData()].
#' @keywords internal
get_parse_data <- function(text, include_text = TRUE, ...) {
  # avoid https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16041
  parse_safely(text, keep.source = TRUE)
  parsed <- parse_safely(text, keep.source = TRUE)
  pd <- utils::getParseData(parsed, includeText = include_text) %>%
    styler_df()
  if (getRversion() < "4.2") {
    is_unicode_parsing_error <- grepl("^\"<U\\+[0-9]+>\"$", pd$text)
    if (any(is_unicode_parsing_error)) {
      rlang::abort(paste0(
        "Can't parse input due to unicode restriction in base R. Please ",
        "upgrade R to >= 4.2 to style this input. ",
        "Context: https://github.com/r-lib/styler/issues/847"
      ))
    }
  }
  pd <- pd %>%
    add_id_and_short()

  parser_version_set(parser_version_find(pd))
  pd
}

#' Add column `pos_id` and `short`
#'
#' Adds column `pos_id` and `short` to a flat parse table.
#' @param pd A flat parse table
#' @keywords internal
add_id_and_short <- function(pd) {
  pd$pos_id <- seq2(1L, nrow(pd))
  pd$short <- substr(pd$text, 1L, 5L)
  pd
}


#' Ensure a correct `text` of all strings and numeric constants
#'
#' Make sure `text` of the tokens `STR_CONST` and `NUM_CONST` is correct and
#' adapt if necessary. We replace offending `text` in the terminal expressions
#' with the text of their parents if their line / col position matches and
#' return an error otherwise.
#' @param pd A parse table.
#' @keywords internal
ensure_correct_txt <- function(pd, text) {
  is_problematic_text <- magrittr::or(
    is_insufficiently_parsed_string(pd),
    is_insufficiently_parsed_number(pd)
  )
  if (!any(is_problematic_text)) {
    return(pd)
  }
  problematic_text <- pd[is_problematic_text, ]
  is_parent_of_problematic_string <- pd$id %in% problematic_text$parent

  is_unaffected_token <- !magrittr::or(
    is_problematic_text, is_parent_of_problematic_string
  )

  pd_with_all_text <- get_parse_data(text, include_text = TRUE)
  parent_cols_for_merge <- c("id", "text", "short", line_col_names())
  parent_of_problematic_text <-
    pd_with_all_text[is_parent_of_problematic_string, parent_cols_for_merge]
  problematic_text$text <- NULL
  problematic_text$short <- NULL
  new_text <- merge(problematic_text, parent_of_problematic_text,
    by.x = "parent",
    by.y = "id",
    suffixes = c("", "parent")
  ) %>%
    styler_df()

  if (!lines_and_cols_match(new_text)) {
    abort(paste(
      "Error in styler:::ensure_correct_txt().",
      "Please file an issue on GitHub (https://github.com/r-lib/styler/issues)"
    ))
  }
  names_to_keep <- setdiff(
    names(new_text),
    paste0(line_col_names(), "parent")
  )
  bind_rows(
    new_text[, names_to_keep],
    pd[is_unaffected_token, ],
    pd[is_parent_of_problematic_string, ]
  ) %>%
    arrange_pos_id()
}


#' Identify strings that were not fully parsed
#'
#' Identifies strings that were not fully parsed due to their vast length.
#' @details
#' The meaning of the variable `is_problematic_string` in the source code
#' changes from "all strings" to "all problematic strings", is partly
#' misleading and this approach was chosen for performance reasons only.
#' @param pd A parse table.
#' @param text The initial code to style.
#' @keywords internal
is_insufficiently_parsed_string <- function(pd) {
  grepl("^\\[", pd$text) & pd$token == "STR_CONST"
}

is_insufficiently_parsed_number <- function(pd) {
  grepl("^0x", pd$text) & pd$token == "NUM_CONST"
}

#' Check whether columns match
#' @keywords internal
#' @noRd
lines_and_cols_match <- function(data) {
  left <- paste0(line_col_names(), "")
  right <- paste0(line_col_names(), "parent")
  identical(
    unlist(data[left], use.names = FALSE),
    unlist(data[right], use.names = FALSE)
  )
}
