#' Obtain a nested parse table from a character vector
#'
#' [utils::getParseData()] is used to obtain a flat parse table from `text`.
#'   Subsequentially, it's representation is changed from a flat table into a
#'   nested parse table with [nest_parse_data()].
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
  parsed <- parse(text = text, keep.source = TRUE)
  parse_data <- tbl_df(utils::getParseData(parsed, includeText = TRUE))
  pd_nested <-
    parse_data %>%
    mutate_(child = ~rep(list(NULL), length(text))) %>%
    mutate_(short = ~substr(text, 1, 5)) %>%
    select_(~short, ~everything()) %>%
    nest_parse_data()

  pd_nested
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
nest_parse_data <- function(pd_flat) {
  if (nrow(pd_flat) <= 1) return(pd_flat)
  split <-
    pd_flat %>%
    mutate_(internal = ~id %in% parent) %>%
    nest_("data", names(pd_flat))

  child <- split$data[!split$internal][[1L]]
  internal <- split$data[split$internal][[1L]]

  internal <- rename_(internal, internal_child = ~child)

  nested <-
    child %>%
    mutate_(parent_ = ~parent) %>%
    nest_(., "child", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_")) %>%
    mutate_(child = ~Map(bind_rows, child, internal_child)) %>%
    mutate_(child = ~lapply(child, arrange_, ~line1, ~col1)) %>%
    select_(~-internal_child) %>%
    select_(~short, ~everything(), ~-text, ~text)

  nest_parse_data(nested)
}

#' Enrich nested parse table with space and linebreak information
#'
#' Uses [create_filler()] in a recursion add space and line break information
#'   separately on every level of nesting.
#' @param pd_nested A nested parse table.
#' @return A nested parse table with two new columns: newlines and spaces.
#' @seealso [create_filler()]
#' @importFrom purrr map
create_filler_nested <- function(pd_nested) {
  if (is.null(pd_nested$child)) return()
  pd_nested <- create_filler(pd_nested)
  pd_nested$child <- map(pd_nested$child, create_filler_nested)
  select_(pd_nested, ~spaces, ~newlines, ~lag_newlines, ~short, ~everything())
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
  raw <- serialize_parse_data_nested_helper(pd_nested, pass_indent = 0) %>%
    unlist() %>%
    paste0(collapse = "") %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
}
