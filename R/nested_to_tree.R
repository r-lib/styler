#' Create a tree from text
#'
#' Create a tree representation from a text.
#' @param text A character vector.
#' @return A data frame.
#' @importFrom purrr when
create_tree <- function(text) {
  compute_parse_data_nested(text) %>%
    pre_visit(c(initialize_attributes)) %>%
    create_node_from_nested_root() %>%
    as.data.frame()
}
#' Convert a nested tibble into a node tree
#'
#' This function is convenient to display all nesting levels of a nested tibble
#'   at once.
#' @param pd_nested A nested tibble.
#' @return An object of class "Node" and "R6".
#' @examples
#' code <- "a <- function(x) { if(x > 1) { 1+1 } else {x} }"
#' nested_pd <- styler:::compute_parse_data_nested(code)
#' initialized <- styler:::pre_visit(nested_pd, c(styler:::initialize_attributes))
#' styler:::create_node_from_nested_root(initialized)
create_node_from_nested_root <- function(pd_nested) {
  n <- data.tree::Node$new("ROOT (token: short_text [lag_newlines/spaces] {pos_id})")
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

  node_info <- paste0(pd_nested$token, ": ", pd_nested$short, " [", pd_nested$lag_newlines, "/", pd_nested$spaces, "] {", pd_nested$pos_id, "}")

  child_nodes <-
    node_info %>%
    map(parent$AddChild)

  map2(pd_nested$child, child_nodes, create_node_from_nested)
  return()
}
