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
  if (length(files) > 0L) {
    cat("Styling ", length(files), " files:\n")
  }

  changed <- map_lgl(
    files, transform_file, fun = transformer, max_char_path = max_char
  )
  communicate_summary(changed, max_char)
  communicate_warning(changed, transformers)
  invisible(data_frame(file = files, changed = changed))
}

#' Transform a file and output a customized message
#'
#' Wraps `enc::transform_lines_enc()` and outputs customized messages.
#' @param max_char_path The number of characters of the longest path. Determines
#'   the indention level of `message_after`.
#' @param message_before The message to print before the path.
#' @param message_after The message to print after the path.
#' @param message_after_if_changed The message to print after `message_after` if
#'   any file was transformed.
#' @inheritParams enc::transform_lines_enc
#' @param ... Further arguments passed to `enc::transform_lines_enc()`.
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
  cat(
    message_before,
    path,
    rep_char(" ", max(0, n_spaces_before_message_after)),
    append = FALSE
  )
  changed <- transform_code(path, fun = fun, verbose = verbose, ...)

  bullet <- ifelse(is.na(changed),
    "warning",
    ifelse(changed,
      "info",
      "tick"
    )
  )

  cli::cat_bullet(
    bullet = bullet
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
  text <- assert_text(text)
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
  flattened_pd <- post_visit(transformed_pd, list(extract_terminals)) %>%
    enrich_terminals(transformers$use_raw_indention) %>%
    apply_ref_indention() %>%
    set_regex_indention(
      pattern          = transformers$reindention$regex_pattern,
      target_indention = transformers$reindention$indention,
      comments_only    = transformers$reindention$comments_only
  )
  serialized_transformed_text <-
    serialize_parse_data_flattened(flattened_pd, start_line = start_line)

  if (can_verify_roundtrip(transformers)) {
    verify_roundtrip(text, serialized_transformed_text)
  }
  serialized_transformed_text
}

#' Apply transformers to a parse table
#'
#' The column `multi_line` is updated (after the line break information is
#' modified) and the rest of the transformers are applied afterwards,
#' The former requires two pre visits and one post visit.
#' @details
#' The order of the transformations is:
#'
#' * Initialization (must be first).
#' * Line breaks (must be before spacing due to indention).
#' * Update of newline and multi-line attributes (must not change afterwards,
#'   hence line breaks must be modified first).
#' * spacing rules (must be after line-breaks and updating newlines and
#'   multi-line).
#' * token manipulation / replacement (is last since adding and removing tokens
#'   will invalidate columns token_after and token_before).
#' * Update indention reference (must be after line breaks).
#'
#' @param pd_nested A nested parse table.
#' @param transformers A list of *named* transformer functions
#' @importFrom purrr flatten
apply_transformers <- function(pd_nested, transformers) {
  transformed_line_breaks <- pre_visit(
    pd_nested,
    c(transformers$initialize, transformers$line_break)
  )

  transformed_updated_multi_line <- post_visit(
    transformed_line_breaks,
    c(set_multi_line, update_newlines)
  )

  transformed_all <- pre_visit(
    transformed_updated_multi_line,
    c(transformers$space, transformers$indention, transformers$token)
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



#' Check whether a roundtip verification can be carried out
#'
#' If scope was set to "line_breaks" or lower (compare [tidyverse_style()]),
#' we can compare the expression before and after styling and return an error if
#' it is not the same.
#' @param transformers The list of transformer functions used for styling.
#'   Needed for reverse engineering the scope.
can_verify_roundtrip <- function(transformers) {
  is.null(transformers$token)
}

#' Verify the styling
#'
#' If scope was set to "line_breaks" or lower (compare [tidyverse_style()]),
#' we can compare the expression before and after styling and return an error if
#' it is not the same. Note that this method ignores comments and no
#' verification can be conducted if scope > "line_breaks".
#' @inheritParams expressions_are_identical
#' @examples
#' styler:::verify_roundtrip("a+1", "a + 1")
#' styler:::verify_roundtrip("a+1", "a + 1 # comments are dropped")
#' \dontrun{
#' styler:::verify_roundtrip("a+1", "b - 3")
#' }
verify_roundtrip <- function(old_text, new_text) {
  if (!expressions_are_identical(old_text, new_text)) {
    msg <- paste(
      "The expression evaluated before the styling is not the same as the",
      "expression after styling. This should not happen. Please file a",
      "bug report on GitHub (https://github.com/r-lib/styler/issues)",
      "using a reprex."
    )
    stop(msg, call. = FALSE)
  }
}

#' Check whether two expressions are identical
#'
#' @param old_text The initial expression in its character representation.
#' @param new_text The styled expression in its character representation.
expressions_are_identical <- function(old_text, new_text) {
  identical(
    parse(text = old_text, keep.source = FALSE),
    parse(text = new_text, keep.source = FALSE)
  )
}
