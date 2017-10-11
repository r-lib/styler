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
  max_char <- min(max(nchar(files), 0), 80)
  if (length(files) > 0) {
    message("Styling ", length(files), " files:")
  }

  changed <- map_lgl(
    files, transform_file, fun = transformer, max_char_path = max_char
  )
  if (!any(changed, na.rm = TRUE)) {
    message("No files changed.")
  } else {
    message("* File changed.")
    message("Please review the changes carefully!")
  }
  invisible(changed)
}

#' Transform a file an give customized message
#'
#' Wraps `utf8::transform_lines_enc()` and gives customized messages.
#' @param max_char_path The number of characters of the longest path. Determines
#'   the indention level of `message_after`.
#' @param message_before The message to print before the path.
#' @param message_after The message to print after the path.
#' @param message_after_if_changed The message to print after `message_after` if
#'   any file was transformed.
#' @inheritParams utf8::transform_lines_enc
#' @param ... Further arguments passed to `utf8::transform_lines_enc()`.
transform_file <- function(path,
                           fun,
                           verbose = FALSE,
                           max_char_path,
                           message_before = "",
                           message_after = " [DONE]",
                           message_after_if_changed = " *",
                           ...) {
  char_after_path <- nchar(message_before) + nchar(path) + 1
  max_char_after_message_path <- nchar(message_before) + max_char_path + 1
  n_spaces_before_message_after <-
    max_char_after_message_path - char_after_path
  message(message_before, path, ".", appendLF = FALSE)
  changed <- utf8::transform_lines_enc(path, fun = fun, verbose = verbose, ...)

  message(
    rep(" ", max(0, n_spaces_before_message_after)),
    message_after,
    if (any(changed, na.rm = TRUE)) message_after_if_changed
  )
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
  force(transformers)
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
  start_line <- find_start_line(pd_nested)
  if (nrow(pd_nested) == 0) {
    warning(
      "Text to style did not contain any tokens. Returning empty string.",
      call. = FALSE
    )
    return("")
  }
  transformed_pd <- apply_transformers(pd_nested, transformers)
  # TODO verify_roundtrip
  flattened_pd <- post_visit(transformed_pd, list(extract_terminals)) %>%
    enrich_terminals(transformers$use_raw_indention) %>%
    apply_ref_indention() %>%
    set_regex_indention(
      pattern          = transformers$reindention$regex_pattern,
      target_indention = transformers$reindention$indention,
      comments_only    = transformers$reindention$comments_only)

  serialized_transformed_text <-
    serialize_parse_data_flattened(flattened_pd, start_line = start_line)
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
    c(transformers$initialize,
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
