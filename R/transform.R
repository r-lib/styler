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
#' @param flat Whether to do the styling with a flat approach or with a nested
#'   approach.
#' @family make transformers
make_transformer <- function(transformers, flat) {
  if (flat) {
    make_transformer_flat(transformers = transformers)
  } else {
    make_transformer_nested(transformers = transformers)
  }
}

#' A Closure to return transformer function
#'
#' Returns a closure that turns `text` into a flat parse table and applies
#'   `transformers`  on it.
#' @inheritParams make_transformer
#' @family make transformers
make_transformer_flat <- function(transformers) {
  function(text) {
    text <- gsub(" +$", "", text)
    text <- gsub("\t", "        ", text)

    pd_flat <- compute_parse_data_flat_enhanced(text)

    # May strip empty lines before EOF
    text <- verify_roundtrip(pd_flat, text)

    transformed_pd_flat <- Reduce(function(x, fun) fun(x),
                                  transformers,
                                  init = pd_flat)

    new_text <- serialize_parse_data_flat(transformed_pd_flat)
    new_text
  }
}

#' Closure to return transformer function
#'
#' Returns a closure that turns `text` into a nested parse table and applies
#'   `transformers`  on it.
#' @inheritParams make_transformer
#' @family make transformers
make_transformer_nested <- function(transformers) {
  function(text) {
    text <- gsub(" +$", "", text)
    text <- gsub("\t", "        ", text)

    pd_nested <- compute_parse_data_nested(text)
    transformed_pd_nested <- visit(pd_nested, transformers)
    # TODO verify_roundtrip
    new_text <- serialize_parse_data_nested(transformed_pd_nested)
    new_text
  }
}

#' Visit'em all
#'
#' Apply a list of functions to each level in a nested parse table.
#' @param pd_nested A nested parse table.
#' @inheritParams visit_one
#' @family visitors
#' @importFrom purrr map
visit <- function(pd_nested, funs) {
  if (is.null(pd_nested)) return()
  pd_transformed <- pd_nested %>%
    visit_one(funs) %>%
    mutate(child = map(child, visit, funs = funs))
  pd_transformed
}

#' Transform a flat parse table with a list of transformers
#'
#' Uses [purrr::reduce()] to apply each function of `funs` sequentially to
#'   `pd_flat`.
#' @param pd_flat A flat parse table.
#' @param funs A list of transformer functions.
#' @family visitors
#' @importFrom purrr reduce
visit_one <- function(pd_flat, funs) {
  reduce(funs, function(x, fun) fun(x),
         .init = pd_flat)
}
