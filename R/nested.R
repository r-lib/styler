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
  parse_data <- tokenize(text)
  pd_nested <-
    parse_data %>%
    mutate_(child = ~rep(list(NULL), length(text))) %>%
    mutate_(short = ~substr(text, 1, 5)) %>%
    select_(~short, ~everything()) %>%
    nest_parse_data()

  pd_nested
}

#' Obtain token table from text
#'
#' [utils::getParseData()] is used to obtain a flat parse table from `text`.
#' @param text A character vector.
#' @return A flat parse table
tokenize <- function(text) {
  parsed <- parse(text = text, keep.source = TRUE)
  parse_data <- as_tibble(utils::getParseData(parsed, includeText = NA))
  parse_data
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
  split <-
    pd_flat %>%
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
  arrange_(bound,  ~line1, ~col1)
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
