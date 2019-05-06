#' Create a tree from text
#'
#' Create a tree representation from a text.
#' @param text A character vector.
#' @inheritParams create_node_from_nested_root
#' @return A data frame.
#' @importFrom purrr when
#' @keywords internal
create_tree <- function(text, structure_only = FALSE) {
  compute_parse_data_nested(text) %>%
    pre_visit(c(default_style_guide_attributes)) %>%
    create_tree_from_pd_with_default_style_attributes(structure_only)
}

create_tree_from_pd_with_default_style_attributes <- function(pd, structure_only = FALSE) {
  pd %>%
    create_node_from_nested_root(structure_only) %>%
    as.data.frame()
}


#' Convert a nested tibble into a node tree
#'
#' This function is convenient to display all nesting levels of a nested tibble
#' at once.
#' @param pd_nested A nested tibble.
#' @param structure_only Whether or not create a tree that represents the
#'   structure of the expression without any information on the tokens. Useful
#'   to check whether two structures are identical.
#' @return An object of class "Node" and "R6".
#' @examples
#' if (rlang::is_installed("data.tree")) {
#'   code <- "a <- function(x) { if(x > 1) { 1+1 } else {x} }"
#'   nested_pd <- styler:::compute_parse_data_nested(code)
#'   initialized <- styler:::pre_visit(nested_pd, c(default_style_guide_attributes))
#'   styler:::create_node_from_nested_root(initialized, structure_only = FALSE)
#' }
#' @keywords internal
create_node_from_nested_root <- function(pd_nested, structure_only) {
  assert_data.tree_installation()
  n <- data.tree::Node$new(ifelse(
    structure_only, "Hierarchical structure",
    "ROOT (token: short_text [lag_newlines/spaces] {pos_id})"
  ))
  create_node_from_nested(pd_nested, n, structure_only)
  n
}
#' Create node from nested parse data
#'
#' @inheritParams create_node_from_nested_root
#' @param parent The parent of the node to be created.
#' @importFrom purrr map2 map
#' @keywords internal
create_node_from_nested <- function(pd_nested, parent, structure_only) {
  if (is.null(pd_nested)) {
    return()
  }

  node_info <- create_node_info(pd_nested, structure_only)

  child_nodes <-
    node_info %>%
    map(parent$AddChild)

  map2(pd_nested$child, child_nodes, create_node_from_nested, structure_only)
}

create_node_info <- function(pd_nested, structure_only) {
  if (structure_only) {
    return(seq2(1L, nrow(pd_nested)))
  }
  paste0(
    pd_nested$token, ": ",
    pd_nested$short, " [",
    pd_nested$lag_newlines, "/",
    pd_nested$spaces, "] {",
    pd_nested$pos_id, "}"
  )
}
