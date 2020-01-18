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
      verify_roundtrip(text, serialized_transformed_text)
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

#' Every expression is an expression itself, but consecutive comments are one
#' expression (potentially)
cache_find_block <- function(pd) {
  seq_len(nrow(pd))
}
