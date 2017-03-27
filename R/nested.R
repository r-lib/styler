#' TODO:
#' - Implement add_ws_to_parse_data_nested()
#'     - Walk tree defined by `leaves`, compute whitespace information
#'     - Store indention depth in a separate column, unaffected by
#'       inter-token space
#' - Implement compute_parse_data_nested_with_ws() as
#'   compute_parse_data_nested() + add_ws_to_parse_data_nested()
#' - Implement serialization of nested parse data
#' - Use compute_parse_data_nested_with_ws() instead of
#'   compute_parse_data_flat_with_ws()
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
  parse_data_nested <-
    parse_data %>%
    mutate_(short = ~substr(text, 1, 5)) %>%
    select_(~short, ~everything()) %>%
    nest_parse_data

  parse_data_nested
}

nest_parse_data <- function(parse_data) {
  if (nrow(parse_data) <= 1) return(parse_data)
  split <-
    parse_data %>%
    mutate_(internal = ~id %in% parent) %>%
    nest_("data", names(parse_data))

  leaves <- split$data[!split$internal][[1L]]
  internal <- split$data[split$internal][[1L]]

  if ("leaves" %in% names(internal)) {
    internal <- rename_(internal, internal_leaves = ~leaves)
  } else {
    internal <- mutate_(internal, internal_leaves = ~vector("list", nrow(internal)))
  }

  nested <-
    leaves %>%
    mutate_(parent_ = ~parent) %>%
    nest_(., "leaves", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_")) %>%
    mutate_(leaves = ~Map(bind_rows, leaves, internal_leaves)) %>%
    mutate_(leaves = ~lapply(leaves, arrange_, ~line1, ~col1)) %>%
    select_(~-internal_leaves) %>%
    select_(~short, ~everything(), ~-text, ~text)

  nest_parse_data(nested)
}
