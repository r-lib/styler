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

#' Serialize Flat Parse Data
#'
#' Collapses a parse table into character vector representation.
#' @param pd_flat A parse table.
#' @details
#'   The function essentially collapses the column text of `pd_flat`
#'   while taking into account space and linebreak information from the columns
#'   newlines and spaces. \cr
#'   Roughly speaking, this is the inverse operation of
#'   [compute_parse_data_flat_enhanced()], which turns a character vector into a
#'   parse table, since `serialize_parse_data_flat()` turns a parse table back
#'   into a character vector.
serialize_parse_data_flat <- function(pd_flat) {
  pd_flat %>%
    summarize_(
      text_ws = ~paste0(
        text, newlines_and_spaces(newlines, spaces),
        collapse = "")) %>%
    .[["text_ws"]] %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
}

