#' Transform files with transformer functions
#'
#' `transform_files` applies transformations to file contents and writes back
#' the result.
#' @param files A character vector with paths to the file that should be
#'   transformed.
#' @inheritParams make_transformer
#' @inheritParams transform_file
#' @section Value:
#' Invisibly returns a data frame that indicates for each file considered for
#' styling whether or not it was actually changed (or would be changed when
#' `dry` is not "off").
#' @keywords internal
transform_files <- function(files, transformers, include_roxygen_examples, base_indention, dry) {
  transformer <- make_transformer(transformers, include_roxygen_examples, base_indention)
  max_char <- min(max(nchar(files), 0), getOption("width"))
  len_files <- length(files)
  if (len_files > 0L && !getOption("styler.quiet", FALSE)) {
    cat("Styling ", len_files, " files:\n")
  }

  changed <- map_lgl(files, transform_file,
    fun = transformer, max_char_path = max_char, dry = dry
  )
  communicate_summary(changed, max_char)
  communicate_warning(changed, transformers)
  new_tibble(list(file = files, changed = changed), nrow = len_files)
}

#' Transform a file and output a customized message
#'
#' Transforms file contents and outputs customized messages.
#' @param max_char_path The number of characters of the longest path. Determines
#'   the indention level of `message_after`.
#' @param message_before The message to print before the path.
#' @param message_after The message to print after the path.
#' @param message_after_if_changed The message to print after `message_after` if
#'   any file was transformed.
#' @inheritParams transform_code
#' @param ... Further arguments passed to [transform_utf8()].
#' @keywords internal
transform_file <- function(path,
                           fun,
                           max_char_path,
                           message_before = "",
                           message_after = " [DONE]",
                           message_after_if_changed = " *",
                           ...,
                           dry) {
  char_after_path <- nchar(message_before) + nchar(path) + 1
  max_char_after_message_path <- nchar(message_before) + max_char_path + 1
  n_spaces_before_message_after <-
    max_char_after_message_path - char_after_path
  if (!getOption("styler.quiet", FALSE)) {
    cat(
      message_before, path,
      rep_char(" ", max(0L, n_spaces_before_message_after)),
      append = FALSE
    )
  }
  changed <- transform_code(path, fun = fun, ..., dry = dry)

  bullet <- ifelse(is.na(changed), "warning", ifelse(changed, "info", "tick"))

  if (!getOption("styler.quiet", FALSE)) {
    cli::cat_bullet(bullet = bullet)
  }
  invisible(changed)
}

#' Closure to return a transformer function
#'
#' This function takes a list of transformer functions as input and
#' returns a function that can be applied to character strings
#' that should be transformed.
#' @param transformers A list of transformer functions that operate on flat
#'   parse tables.
#' @param include_roxygen_examples Whether or not to style code in roxygen
#'   examples.
#' @inheritParams parse_transform_serialize_r
#' @keywords internal
#' @importFrom purrr when
make_transformer <- function(transformers,
                             include_roxygen_examples,
                             base_indention,
                             warn_empty = TRUE) {
  force(transformers)
  assert_transformers(transformers)

  function(text) {
    text <- trimws(text, which = "right")
    should_use_cache <- cache_is_activated()

    if (should_use_cache) {
      use_cache <- is_cached(
        text, transformers,
        cache_more_specs(
          include_roxygen_examples = include_roxygen_examples,
          base_indention = base_indention
        )
      )

      if (!is.null(attr(use_cache, "text"))) {
        text <- attr(use_cache, "text")
      }
    } else {
      use_cache <- FALSE
    }

    if (!use_cache) {
      transformed_code <- text %>%
        parse_transform_serialize_r(transformers,
          base_indention = base_indention,
          warn_empty = warn_empty
        ) %>%
        when(
          include_roxygen_examples ~
          parse_transform_serialize_roxygen(.,
            transformers = transformers, base_indention = base_indention
          ),
          ~.
        )
      if (should_use_cache) {
        specs <- cache_more_specs(include_roxygen_examples, base_indention)
        cache_write(transformed_code, transformers, specs)
        cache_write(text, transformers, specs, transformed_code)
      }
      transformed_code
    } else {
      text
    }
  }
}

#' Parse, transform and serialize roxygen comments
#'
#' Splits `text` into roxygen code examples and non-roxygen code examples and
#' then maps over these examples by applying
#' [style_roxygen_code_example()].
#' @section Hierarchy:
#' Styling involves splitting roxygen example code into segments, and segments
#' into snippets. This describes the process for input of
#' [parse_transform_serialize_roxygen()]:
#'
#' - Splitting code into roxygen example code and other code. Downstream,
#'   we are only concerned about roxygen code. See
#'   [parse_transform_serialize_roxygen()].
#' - Every roxygen example code can have zero or more
#'   dontrun / dontshow / donttest sequences. We next create segments of roxygen
#'   code examples that contain at most one of these. See
#'   [style_roxygen_code_example()].
#' - We further split the segment that contains at most one dont* sequence into
#'   snippets that are either don* or not. See
#'   [style_roxygen_code_example_segment()].
#'
#' Finally, that we have roxygen code snippets that are either dont* or not,
#' we style them in [style_roxygen_example_snippet()] using
#' [parse_transform_serialize_r()].
#' @importFrom purrr map_at flatten_chr
#' @keywords internal
parse_transform_serialize_roxygen <- function(text, transformers, base_indention) {
  roxygen_seqs <- identify_start_to_stop_of_roxygen_examples_from_text(text)
  if (length(roxygen_seqs) < 1L) {
    return(text)
  }
  if (!rlang::is_installed("roxygen2")) {
    rlang::abort(paste0(
      "To style roxygen code examples, you need to have the package ",
      "`{roxygen2}` installed. To exclude them from styling, set ",
      "`include_roxygen_examples = FALSE`."
    ))
  }
  split_segments <- split_roxygen_segments(text, unlist(roxygen_seqs))
  map_at(split_segments$separated, split_segments$selectors,
    style_roxygen_code_example,
    transformers = transformers,
    base_indention = base_indention
  ) %>%
    flatten_chr()
}


#' Split text into roxygen and non-roxygen example segments
#'
#' @param text Roxygen comments
#' @param roxygen_examples Integer sequence that indicates which lines in `text`
#'   are roxygen examples. Most conveniently obtained with
#'   [identify_start_to_stop_of_roxygen_examples_from_text].
#' @return
#' A list with two elements:
#'
#' * A list that contains elements grouped into roxygen and non-roxygen
#'   sections. This list is named `separated`.
#' * An integer vector with the indices that correspond to roxygen code
#'   examples in `separated`.
#' @importFrom rlang seq2
#' @keywords internal
split_roxygen_segments <- function(text, roxygen_examples) {
  if (is.null(roxygen_examples)) {
    return(list(separated = list(text), selectors = NULL))
  }
  all_lines <- seq2(1L, length(text))
  active_segment <- as.integer(all_lines %in% roxygen_examples)
  segment_id <- cumsum(abs(c(0L, diff(active_segment)))) + 1L
  separated <- split(text, factor(segment_id))
  restyle_selector <- ifelse(roxygen_examples[1] == 1L, odd_index, even_index)

  list(separated = separated, selectors = restyle_selector(separated))
}

#' Parse, transform and serialize text
#'
#' Wrapper function for the common three operations.
#' @param warn_empty Whether or not a warning should be displayed when `text`
#'   does not contain any tokens.
#' @inheritParams compute_parse_data_nested
#' @inheritParams parse_transform_serialize_r_block
#' @seealso [parse_transform_serialize_roxygen()]
#' @importFrom rlang abort
#' @keywords internal
parse_transform_serialize_r <- function(text,
                                        transformers,
                                        base_indention,
                                        warn_empty = TRUE) {
  more_specs <- cache_more_specs(
    include_roxygen_examples = TRUE, base_indention = base_indention
  )

  text <- assert_text(text)
  pd_nested <- compute_parse_data_nested(text, transformers, more_specs)
  if (nrow(pd_nested) == 0) {
    if (warn_empty) {
      warn("Text to style did not contain any tokens. Returning empty string.")
    }
    return("")
  }
  transformers <- transformers_drop(
    if (getRversion() < 3.4) text else pd_nested$text[!pd_nested$is_cached],
    transformers
  )

  text_out <- pd_nested %>%
    split(pd_nested$block) %>%
    unname() %>%
    map2(find_blank_lines_to_next_block(pd_nested),
      parse_transform_serialize_r_block,
      transformers = transformers,
      base_indention = base_indention
    ) %>%
    unlist()

  if (can_verify_roundtrip(transformers)) {
    verify_roundtrip(text, text_out)
  }
  text_out <- convert_newlines_to_linebreaks(text_out)
  if (cache_is_activated()) {
    cache_by_expression(text_out, transformers, more_specs = more_specs)
  }
  text_out
}


#' Remove transformers that are not needed
#'
#' The goal is to speed up styling by removing all rules that are only
#' applicable in contexts that don't occur often, e.g. for most code, we don't
#' expect ";" to be in it, so we don't need to apply `resolve_semicolon()` on
#' every *nest*.
#' @param text Text to parse. Can also be the column `text` of the output of
#'   [compute_parse_data_nested()], where each element is a token (instead of a
#'   line).
#' @param transformers the transformers.
#' @keywords internal
#' @seealso specify_transformers_drop
transformers_drop <- function(text, transformers) {
  is_colon <- text == ";"
  if (any(is_colon)) {
    # ; can only be parsed when on the same line as other token, not the case
    # here since text is output of compute_parse_data_nested.
    text <- c(text[!is_colon], "1;")
  }
  token <- unique(tokenize(text)$token)
  for (scope in c("line_break", "space", "token", "indention")) {
    rules <- transformers$transformers_drop[[scope]]
    for (rule in names(rules)) {
      if (!any(rules[[rule]] %in% token)) {
        transformers[[scope]][rule] <- NULL
      }
    }
  }
  transformers
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
#' * indention.
#' * token manipulation / replacement (is last since adding and removing tokens
#'   will invalidate columns token_after and token_before).
#' * Update indention reference (must be after line breaks).
#'
#' @param pd_nested A nested parse table.
#' @param transformers A list of *named* transformer functions
#' @importFrom purrr flatten
#' @keywords internal
apply_transformers <- function(pd_nested, transformers) {
  transformed_updated_multi_line <- post_visit(
    pd_nested,
    c(
      transformers$initialize, transformers$line_break, set_multi_line,
      if (length(transformers$line_break) != 0) update_newlines
    )
  )

  transformed_all <- pre_visit(
    transformed_updated_multi_line,
    c(transformers$space, transformers$indention, transformers$token)
  )

  transformed_absolute_indent <- context_to_terminals(
    transformed_all,
    outer_lag_newlines = 0L,
    outer_indent = 0L,
    outer_spaces = 0L,
    outer_indention_refs = NA
  )
  transformed_absolute_indent
}



#' Check whether a round trip verification can be carried out
#'
#' If scope was set to "line_breaks" or lower (compare [tidyverse_style()]),
#' we can compare the expression before and after styling and return an error if
#' it is not the same.
#' @param transformers The list of transformer functions used for styling.
#'   Needed for reverse engineering the scope.
#' @keywords internal
can_verify_roundtrip <- function(transformers) {
  length(transformers$token) == 0
}

#' Verify the styling
#'
#' If scope was set to "line_breaks" or lower (compare [tidyverse_style()]),
#' we can compare the expression before and after styling and return an error if
#' it is not the same. Note that this method ignores roxygen code examples and
#' comments and no verification can be conducted if tokens are in the styling
#' scope.
#' @inheritParams expressions_are_identical
#' @importFrom rlang abort
#' @examples
#' styler:::verify_roundtrip("a+1", "a + 1")
#' styler:::verify_roundtrip("a+1", "a + 1 # comments are dropped")
#' \dontrun{
#' styler:::verify_roundtrip("a+1", "b - 3")
#' }
#' @keywords internal
verify_roundtrip <- function(old_text, new_text) {
  if (!expressions_are_identical(old_text, new_text)) {
    msg <- paste(
      "The expression evaluated before the styling is not the same as the",
      "expression after styling. This should not happen. Please file a",
      "bug report on GitHub (https://github.com/r-lib/styler/issues)",
      "using a reprex."
    )
    abort(msg)
  }
}

#' Check whether two expressions are identical
#'
#' @param old_text The initial expression in its character representation.
#' @param new_text The styled expression in its character representation.
#' @keywords internal
expressions_are_identical <- function(old_text, new_text) {
  identical(
    parse_safely(old_text, keep.source = FALSE),
    parse_safely(new_text, keep.source = FALSE)
  )
}
