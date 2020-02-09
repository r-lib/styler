#' Parse, transform and serialize a nested parse table
#'
#' We process blocks of nested parse tables for speed. See [cache_find_block()]
#' for details on how a top level nest is split into blocks.
#' @param pd_nested A block of the nested parse table.
#' @param start_line The line number on which the code starts.
#' @inheritParams apply_transformers
#' @keywords internal
parse_transform_serialize_r_block <- function(pd_nested,
                                              start_line,
                                              transformers) {
  if (!all(pd_nested$is_cached, na.rm = TRUE) || !cache_is_activated()) {
    transformed_pd <- apply_transformers(pd_nested, transformers)
    flattened_pd <- post_visit(transformed_pd, list(extract_terminals)) %>%
      enrich_terminals(transformers$use_raw_indention) %>%
      apply_ref_indention() %>%
      set_regex_indention(
        pattern = transformers$reindention$regex_pattern,
        target_indention = transformers$reindention$indention,
        comments_only = transformers$reindention$comments_only
      )
    serialized_transformed_text <- serialize_parse_data_flattened(flattened_pd)
  } else {
    serialized_transformed_text <- map2(
      c(0, find_blank_lines_to_next_expr(pd_nested)[-1] - 1L),
      pd_nested$text,
      ~ c(rep("", .x), .y)
    ) %>%
      unlist()
  }
  c(rep("", start_line - 1), serialized_transformed_text)
}

#' Find the groups of expressions that should be processed together
#'
#' Every expression is an expression itself, Expressions on same line are in
#' same block.
#' Multiple expressions can sit on one row, e.g. in line comment and commands
#' separated with ";". This creates a problem when processing each expression
#' separately because when putting them together, we need complicated handling
#' of line breaks between them, as it is not apriori clear that there is a line
#' break separating them. To avoid this, we put top level expressions that sit
#' on the same line into one block, so the assumption that there is a line break
#' between each block of expressions holds.
#' @param pd A top level parse table.
#' @details
#' we want to for turning points:
#' - change in cache state is a turning point
#' - expressions that are not on a new line cannot be a turning point. In this
#'   case, the turning point is moved to the first expression on the line
#' @param pd A top level nest.
#' @keywords internal
cache_find_block <- function(pd) {
  first_after_cache_state_switch <- pd$is_cached != lag(pd$is_cached, default = !pd$is_cached[1])

  not_first_on_line <- find_blank_lines_to_next_expr(pd) == 0
  invalid_turning_point_idx <- which(
    not_first_on_line & first_after_cache_state_switch
  )

  first_on_line_idx <- which(!not_first_on_line)
  valid_replacements <- map_int(invalid_turning_point_idx, function(x) {
    last(which(x > first_on_line_idx))
  })
  sort(unique(c(
    setdiff(which(first_after_cache_state_switch), invalid_turning_point_idx),
    valid_replacements
  ))) %>%
    unwhich(nrow(pd)) %>%
    cumsum()
}


#' Find blank lines
#'
#' What number of line breaks lay between the expressions?
#' @param pd_nested A nested parse table.
#' @return The line number on which the first token occurs.
#' @keywords internal
find_blank_lines_to_next_expr <- function(pd_nested) {
  pd_nested$line1 - lag(pd_nested$line2, default = 0)
}

#' Number of lines between cache blocks
#'
#' This is relevant when putting expressions together into a block and preserve
#' blank lines between them. Note that because code does not need to start on
#' line 1, the first element of the output is the number of lines until the
#' first block.
#' @param pd A top level nest.
find_blank_lines_to_next_block <- function(pd) {
  block_boundary <- pd$block != lag(pd$block, default = 0)
  find_blank_lines_to_next_expr(pd)[block_boundary]
}
