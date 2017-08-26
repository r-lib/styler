#' Obtain a nested parse table from a character vector
#'
#' Parses `text` to a flat parse table and subsequently changes its
#'   representation into a nested parse table with
#'   [nest_parse_data()].
#' @param text A character vector to parse.
#' @return A nested parse table. Apart from the columns provided by
#'   `utils::getParseData()`, a column "short" with the first five characters of
#'   "text" is added, the nested subtibbles are in column "child".
compute_parse_data_nested <- function(text) {
  parse_data <- tokenize(text) %>%
    add_terminal_token_before() %>%
    add_terminal_token_after()

  parse_data$child <- rep(list(NULL), length(parse_data$text))
  pd_nested <- parse_data %>%
    nest_parse_data() %>%
    flatten_operators()

  pd_nested
}

#' Obtain token table from text
#'
#' [utils::getParseData()] is used to obtain a flat parse table from `text`.
#' @param text A character vector.
#' @return A flat parse table
tokenize <- function(text) {
  parsed <- parse(text = text, keep.source = TRUE)
  parse_data <- as_tibble(utils::getParseData(parsed, includeText = NA)) %>%
    enhance_mapping_special()
  parse_data$short <- substr(parse_data$text, 1, 5)
  parse_data
}

#' Enhance the mapping of text to the token "SPECIAL"
#'
#' Map text corresponding to the token "SPECIAL" to a (more) unique token
#'   description.
#' @param pd A parse table.
enhance_mapping_special <- function(pd) {
  pd$token <- with(pd, case_when(
      token != "SPECIAL" ~ token,
      text == "%>%" ~ special_and("PIPE"),
      text == "%in%" ~ special_and("IN"),
      TRUE ~ special_and("OTHER")
    ))
  pd
}

special_and <- function(text) {
  paste0("SPECIAL-", text)
}

#' Add information about previous / next token to each terminal
#'
#' @param pd_flat A flat parse table.
#' @name add_token_terminal
NULL

#' @rdname add_token_terminal
add_terminal_token_after <- function(pd_flat) {
  terminals <- pd_flat %>%
    filter(terminal) %>%
    arrange(line1, col1)

  data_frame(id = terminals$id,
             token_after = lead(terminals$token, default = "")) %>%
    left_join(pd_flat, ., by = "id")
}

#' @rdname add_token_terminal
add_terminal_token_before <- function(pd_flat) {
  terminals <- pd_flat %>%
    filter(terminal) %>%
    arrange(line1, col1)

  data_frame(id = terminals$id,
             token_before = lag(terminals$token, default = "")) %>%
    left_join(pd_flat, ., by = "id")
}

#' Helper for setting spaces
#'
#' @param spaces_after_prefix An integer vector with the number of spaces
#'   after the prefix.
#' @param force_one Whether spaces_after_prefix should be set to one in all
#'   cases.
#' @return An integer vector of length spaces_after_prefix, which is either
#'   one (if `force_one = TRUE`) or `space_after_prefix` with all values
#'   below one set to one.
set_spaces <- function(spaces_after_prefix, force_one) {
  if (force_one) {
    n_of_spaces <- rep(1, length(spaces_after_prefix))
  } else {
    n_of_spaces <- pmax(spaces_after_prefix, 1L)
  }
  n_of_spaces
}

#' Nest a flat parse table
#'
#' `nest_parse_data` groups `pd_flat` into a parse table with tokens that are
#'   a parent to other tokens (called internal) and such that are not (called
#'   child). Then, the token in child are joined to their parents in internal
#'   and all token information of the children is nested into a column "child".
#'   This is done recursively until we are only left with a nested tibble that
#'   contains one row: The nested parse table.
#' @param pd_flat A flat parse table including both terminals and non-terminals.
#' @seealso [compute_parse_data_nested()]
#' @return A nested parse table.
#' @importFrom purrr map2
nest_parse_data <- function(pd_flat) {
  if (all(pd_flat$parent <= 0)) return(pd_flat)
  pd_flat$internal <- with(pd_flat, (id %in% parent) | (parent <= 0))
  split_data <- split(pd_flat, pd_flat$internal)

  child <- split_data$`FALSE`
  internal <- split_data$`TRUE`

  internal <- rename_(internal, internal_child = ~child)

  child$parent_ <- child$parent
  joined <-
    child %>%
    nest_(., "child", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_"))
  nested <- joined
  nested$child <- map2(nested$child, nested$internal_child, combine_children)
  nested <- nested[, setdiff(names(nested), "internal_child")]
  nest_parse_data(nested)
}

#' Combine child and internal child
#'
#' Binds two parse tables together and arranges them so that the tokens are in
#'   the correct order.
#' @param child A parse table or `NULL`.
#' @param internal_child A parse table or `NULL`.
#' @details Essentially, this is a wrapper around [dplyr::bind_rows()], but
#'   returns `NULL` if the result of [dplyr::bind_rows()] is a data frame with
#'   zero rows.
combine_children <- function(child, internal_child) {
  bound <- bind_rows(child, internal_child)
  if (nrow(bound) == 0) return(NULL)
  bound[order(bound$line1, bound$col1), ]

}

#' Get the start right
#'
#' On what line does the first token occur?
#' @param pd A parse table.
#' @return The line number on which the first token occurs.
start_on_line <- function(pd) {
  pd$line1[1]
}
