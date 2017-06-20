#' Convert a nested tibble into a node tree
#'
#' This function is convenient to display all nesting levels of a nested tibble
#'   at once.
#' @param pd_nested A nested tibble.
#' @return An object of class "Node" and "R6".
#' @examples
#' library("magrittr")
#' code <- "a <- function(x) { if(x > 1) { 1+1 } else {x} }"
#' l1 <- styler:::compute_parse_data_nested(code) %>%
#'   styler:::create_filler_nested() %>%
#'   styler:::create_node_from_nested_root()
create_node_from_nested_root <- function(pd_nested) {
  n <- data.tree::Node$new("xxx")
  create_node_from_nested(pd_nested, n)
  n
}

#' Create node from nested parse data
#'
#' @inheritParams create_node_from_nested_root
#' @param parent The parent of the node to be created.
#' @importFrom purrr map2 map
create_node_from_nested <- function(pd_nested, parent) {
  if (is.null(pd_nested))
    return()

  node_info <-
    pd_nested %>%
    transmute(formatted = paste0(token, ": ", short, " [", newlines, "/", spaces, "]")) %>%
    .[["formatted"]]

  child_nodes <-
    node_info %>%
    map(parent$AddChild)

  map2(pd_nested$child, child_nodes, create_node_from_nested)
  return()
}
