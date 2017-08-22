#' Transform files with transformer functions
#'
#' `transform_files` applies transformations to file contents and writes back
#'   the result.
#' @param files A character vector with paths to the file that should be
#'   transformed.
#' @inheritParams make_transformer
#' @return A logical value that indicates whether or not any file was changed is
#'   returned invisibly. If files were changed, the user is informed to
#'   carefully inspect the changes via a message sent to the console.
transform_files <- function(files, transformers) {
  transformer <- make_transformer(transformers)

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
    message("Please review the changes carefully!")
  }
  invisible(changed)
}
#' Closure to return a transformer function
#'
#' This function takes a list of transformer functions as input and
#'  returns a function that can be applied to character strings
#'  that should be transformed.
#' @param transformers A list of transformer functions that operate on flat
#'   parse tables.
make_transformer <- function(transformers) {
  function(text) {
    transformed_text <- parse_transform_serialize(text, transformers)
    transformed_text

  }
}

#' Parse, transform and serialize text
#'
#' Wrapper function for the common three operations.
#' @inheritParams compute_parse_data_nested
#' @inheritParams apply_transformers
parse_transform_serialize <- function(text, transformers) {
  pd_nested <- compute_parse_data_nested(text)
  transformed_pd <- apply_transformers(pd_nested, transformers)
  # TODO verify_roundtrip
  flattened_pd <- post_visit(transformed_pd, list(extract_terminals)) %>%
    enrich_terminals(transformers$use_raw_indention) %>%
    apply_ref_indention()

  serialized_transformed_text <- serialize_parse_data_flattened(flattened_pd)
  serialized_transformed_text
}


#' Apply transformers to a parse table
#'
#' Depending on whether `transformers` contains functions to modify the
#'   line break information, the column `multi_line` is updated (after
#'   the line break information is modified) and
#'   the rest of the transformers is applied afterwards, or (if line break
#'   information is not to be modified), all transformers are applied in one
#'   step. The former requires two pre visits and one post visit, the latter
#'   only one pre visit.
#' @param pd_nested A nested parse table.
#' @param transformers A list of *named* transformer functions
#' @importFrom purrr flatten
apply_transformers <- function(pd_nested, transformers) {
  transformed_line_breaks <- pre_visit(
    pd_nested,
    c(transformers$filler,
    transformers$line_break)
  )

  transformed_updated_multi_line <- post_visit(
    transformed_line_breaks,
    c(set_multi_line, update_newlines)
  )

  transformed_all <- pre_visit(
    transformed_updated_multi_line,
    c(transformers$space, transformers$token, transformers$indention)
  )

  transformed_absolute_indent <- context_to_terminals(
    transformed_all,
    outer_lag_newlines = 0,
    outer_indent = 0,
    outer_spaces = 0,
    outer_indention_refs = NA
  )

  transformed_absolute_indent

}
