#' Save parsing from text
#'
#' Parses text safely, i.e. throws an informative error if EOL style does not
#' match LF or indicates the exact position where the parsing failed. Note
#' that we can only detect wrong EOL style if it occurs on the first line
#' already.
#' @param text Text to parse.
#' @param ... Parameters passed to [base::parse()]
#' @importFrom rlang abort with_handlers warn
#' @keywords internal
#' @examples
#' \dontrun{
#' styler:::parse_safely("a + 3 -4 -> x\r\n glück + 1")
#' # This cannot be detected as a EOL style problem because the first
#' # line ends as expected with \n
#' styler:::parse_safely("a + 3 -4 -> x\nx + 2\r\n glück + 1")
#' }
#' styler:::parse_safely("a + 3 -4 -> \n glück + 1")
parse_safely <- function(text, ...) {
  tried_parsing <- with_handlers(
    parse(text = text, ...),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(tried_parsing, "error")) {
    if (has_crlf_as_first_line_sep(tried_parsing$message, text)) {
      abort(paste0(
        "The code to style seems to use Windows style line endings (CRLF). ",
        "styler currently only supports Unix style line endings (LF). ",
        "Please change the EOL character in your editor to Unix style and try again.",
        "\nThe parsing error was:\n", tried_parsing$message
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
  split <- strsplit(message, ":", fixed = TRUE)[[1]]
  if (length(split) > 1L && split[1] == "<text>") {
    start_char <- as.numeric(split[3])
    offending_line <- initial_text[as.integer(split[2])]
    if (!is.na(offending_line)) {
      if (substr(offending_line, start_char, start_char + 1) == "\r\n") {
        return(TRUE)
      }
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
  parse_safely(text, keep.source = TRUE)
  parsed <- parse_safely(text, keep.source = TRUE)
  pd <- as_tibble(utils::getParseData(parsed, includeText = include_text)) %>%
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
#' @importFrom rlang abort
#' @keywords internal
ensure_correct_str_txt <- function(pd, text) {
  ensure_valid_pd(pd)
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
    by.x = "parent",
    by.y = "id",
    suffixes = c("", "parent")
  ) %>%
    as_tibble()

  if (!lines_and_cols_match(new_strings)) {
    abort(paste(
      "Error in styler:::ensure_correct_str_txt().",
      "Please file an issue on GitHub (https://github.com/r-lib/styler/issues)",
    ))
  }
  names_to_keep <- setdiff(
    names(new_strings),
    paste0(line_col_names(), "parent")
  )
  bind_rows(
    new_strings[, names_to_keep],
    pd[is_unaffected_token, ],
    pd[is_parent_of_problematic_string, ]
  ) %>%
    arrange(pos_id)
}

#' Ensure that the parse data is valid
#'
#' Test whether all non-terminals have at least one child and throw an error
#' otherwise. As this is check is rather expensive, it is only
#' carried out for configurations we have good reasons to expect problems.
#' @param pd A parse table.
#' @importFrom rlang abort
#' @keywords internal
ensure_valid_pd <- function(pd) {
  if (getRversion() < "3.2") {
    non_terminals <- pd %>%
      filter(terminal == FALSE)
    valid_pd <- non_terminals$id %>%
      map_lgl(~ .x %in% pd$parent) %>%
      all()
    if (!valid_pd) {
      abort(paste(
        "The parse data is not valid and the problem is most likely related",
        "to the parser in base R. Please install R >= 3.2 and try again."
      ))
    }
  }
  TRUE
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
  left <- paste0(line_col_names(), "")
  right <- paste0(line_col_names(), "parent")
  map2_lgl(left, right,
    two_cols_match,
    data = data
  ) %>%
    all()
}
