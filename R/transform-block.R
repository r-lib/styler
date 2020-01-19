#' @param pd_nested A block of the nested parse table
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
    serialized_transformed_text <-
      serialize_parse_data_flattened(flattened_pd, start_line = start_line)

    if (can_verify_roundtrip(transformers)) {
      #TODO do recreate or pass text here
      #verify_roundtrip(text, serialized_transformed_text)
    }
    R.cache::generateCache(
      key = cache_make_key(serialized_transformed_text, transformers),
      dirs = cache_dir_default()
    ) %>%
      file.create()
    serialized_transformed_text
  } else {
    pd_nested$text
  }
}

#' Find the groups of expressions that should be processed together
#'
#' Every expression is an expression itself, Expressions on same line are in
#' same block.
#' Multiple expressions can sit on one row, e.g. in line comment and commands
#' seperated with ";". This creates a problem when processing each expression
#' separately because when putting them together, we need complicated handling
#' of line breaks between them, as it is not apriory clear that there is a line
#' break separating them. To avoid this, we put top level expressions that sit
#' on the same line into one block, so the assumption that there is a line break
#' between each block of expressions holds.
#' @param pd A top level nest.
cache_find_block <- function(pd) {
  on_new_line <- find_blank_lines_to_next_expr(pd) != 0
  cumsum(on_new_line)
}


#' Find blank lines
#'
#' What number of line breaks lay between the expressions?
#' @param pd_nested A nested parse table.
#' @return The line number on which the first token occurs.
#' @keywords internal
find_blank_lines_to_next_expr <- function(pd_nested) {
  # TODO think about naming: prefix with cache here also or just ui functions?
  pd_nested$line1 - lag(pd_nested$line2, default = 0)
}

#' Number of lines between cache blocks
#'
#' This is relevant when putting expressions together into a block and preserve
#' blank lines between them.
#' @param pd A top level nest.
find_blank_lines_to_next_block <- function(pd) {
  block_boundary <- pd$block != lag(pd$block, default = 0)
  # TODO everywhere: block is not ambiguous. use cache block since we also have
  # block_id and other things in other places
  find_blank_lines_to_next_expr(pd)[block_boundary]
}

