#' Obtain a nested parse table from a character vector
#'
#' Parses `text` to a flat parse table and subsequently changes its
#'   representation into a nested parse table with
#'   [nest_parse_data()].
#' @param text A character vector to parse.
#' @return A nested parse table. Apart from the columns provided by
#'   `utils::getParseData()`, a column "short" with the first five characters of
#'   "text" is added, the nested subtibbles are in column "child".
#' TODO:
#' - Implement enhance_parse_data_nested()
#'     - Walk tree defined by `child`, compute whitespace information
#'     - Store indention depth in a separate column, unaffected by
#'       inter-token space
#' - Implement compute_parse_data_nested_with_ws() as
#'   compute_parse_data_nested() + enhance_parse_data_nested()
#' - Implement serialization of nested parse data
#' - Use compute_parse_data_nested_with_ws() instead of
#'   compute_parse_data_flat_enhanced()
#' - Perform all transformations on hierarchical structure
#'     - Compute text for a sub-element
#' - Compute indentation
#'     - Braces
#'     - Function calls
#'     - Function definitions
#' - Remove `includeText = TRUE`
compute_parse_data_nested <- function(text) {
  parse_data <- tokenize(text) %>%
    add_terminal_token_before() %>%
    add_terminal_token_after()

  pd_nested <- parse_data %>%
    mutate_(child = ~rep(list(NULL), length(text))) %>%
    mutate_(short = ~substr(text, 1, 5)) %>%
    select_(~short, ~everything()) %>%
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
  parse_data
}

#' Enhance the mapping of text to the token "SPECIAL"
#'
#' Map text corresponding to the token "SPECIAL" to a (more) unique token
#'   description.
enhance_mapping_special <- function(pd) {
  pd %>%
    mutate(token = case_when(
      token != "SPECIAL" ~ token,
      text == "%>%" ~ special_and("PIPE"),
      text == "%in%" ~ special_and("IN"),
      TRUE ~ special_and("OTHER")
    ))
}

special_and <- function(text) {
  paste0("SPECIAL-", text)
}


#' lookup which new tokens were created from "SPECIAL"
#'
#' @importFrom purrr map_chr
lookup_new_special <- function(regex = NA) {
  new_special <- c("PIPE", "IN", "OTHER")

  potential_regex <- grep(regex, new_special, value = TRUE, ignore.case = TRUE)
  if (is.na(regex)) {
    mapping <- new_special
  } else if (length(potential_regex) > 0) {
    mapping <- potential_regex
  } else {
    return(NA)
  }
  map_chr(mapping, special_and)
}


#' Add information about previous / next token to each terminal
#'
#' @param pd_flat A flat parse table.
#' @name add_token_terminal
NULL

#' @rdname add_token_terminal
add_terminal_token_after <- function(pd_flat) {
  pd_flat %>%
    filter(terminal) %>%
    arrange(line1, col1) %>%
    transmute(id = id, token_after = lead(token, default = "")) %>%
    left_join(pd_flat, ., by = "id")
}

#' @rdname add_token_terminal
add_terminal_token_before <- function(pd_flat) {
  pd_flat %>%
    filter(terminal) %>%
    arrange(line1, col1) %>%
    transmute(id = id, token_before = lag(token, default = "")) %>%
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
    n_of_spaces <- if_else(spaces_after_prefix < 1L, 1L, spaces_after_prefix)
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
  split <- pd_flat %>%
    mutate_(internal = ~ (id %in% parent) | (parent <= 0)) %>%
    nest_("data", names(pd_flat))

  child <- split$data[!split$internal][[1L]]
  internal <- split$data[split$internal][[1L]]

  internal <- rename_(internal, internal_child = ~child)

  nested <-
    child %>%
    mutate_(parent_ = ~parent) %>%
    nest_(., "child", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_")) %>%
    mutate_(child = ~map2(child, internal_child, combine_children)) %>%
    select_(~-internal_child) %>%
    select_(~short, ~everything(), ~-text, ~text)

  nest_parse_data(nested)
}

#' Combine child and internal child
#'
#' binds two parse tables together and arranges them so that the tokens are in
#'   the correct order.
#' @param child A parse table or `NULL`.
#' @param internal_child A parse table or `NULL`.
#' @details Essentially, this is a wrapper around [dplyr::bind_rows()], but
#'   returns `NULL` if the result of [dplyr::bind_rows()] is a data frame with
#'   zero rows.
combine_children <- function(child, internal_child) {
  bound <- bind_rows(child, internal_child)
  if (nrow(bound) == 0) return(NULL)
  arrange_(bound, ~line1, ~col1)
}


#' Serialize a nested parse table
#'
#' Helper function that recursively extracts terminals from a nested tibble.
#' @param pd_nested A nested parse table.
#' @param pass_indent Level of indention of a token.
#' @return A character vector with all terminal tokens in `pd_nested` plus
#'   the appropriate amount of white spaces and line breaks are inserted between
#'   them.
#' @importFrom purrr pmap
serialize_parse_data_nested_helper <- function(pd_nested, pass_indent) {
  out <- pmap(list(pd_nested$terminal, pd_nested$text, pd_nested$child,
                   pd_nested$spaces, pd_nested$lag_newlines, pd_nested$indent),
              function(terminal, text, child, spaces, lag_newlines, indent) {
                total_indent <- pass_indent + indent
                preceding_linebreak <- if_else(lag_newlines > 0, 1, 0)
                if (terminal) {
                  c(add_newlines(lag_newlines),
                    add_spaces(total_indent * preceding_linebreak),
                    text,
                    add_spaces(spaces))
                } else {
                  c(add_newlines(lag_newlines),
                    add_spaces(total_indent * preceding_linebreak),
                    serialize_parse_data_nested_helper(child, total_indent),
                    add_spaces(spaces))
                }
              }
  )
  out
}

#' Serialize a nested parse table
#'
#' Collapses a nested parse table into its character vector representation.
#' @param pd_nested A nested parse table with line break, spaces and indention
#'   information.
#' @return A character string.
serialize_parse_data_nested <- function(pd_nested) {
  out <- c(add_newlines(start_on_line(pd_nested) - 1),
           serialize_parse_data_nested_helper(pd_nested, pass_indent = 0)) %>%
    unlist() %>%
    paste0(collapse = "") %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
  out
}

#' Get the start right
#'
#' On what line does the first token occur?
#' @param pd A parse table.
#' @return The line number on which the first token occurs.
start_on_line <- function(pd) {
  pd$line1[1]
}
