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
#' @param transformers A list of transformer functions that operate on parse
#'   tables.
#' @param flat Whether to do the styling with a flat approach or with a nested
#'   approach.
make_transformer <- function(transformers, flat = FALSE) {
  if (flat) {
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

  } else {
    function(text) {
      text <- gsub(" +$", "", text)
      text <- gsub("\t", "        ", text)

      pd_nested <- styler:::compute_parse_data_nested(text) %>%
        styler:::create_filler_nested() %>%
        styler:::indent_round_nested()

      #' TODO verify_roundtrip
      transformed_pd_nested <- Reduce(
        transform_nested,
        transformers,
        init = pd_nested)

      new_text <- styler:::serialize_parse_data_nested(transformed_pd_nested)
      new_text
    }

  }
}


#' Transform nested tibble with a transformer function
#'
#' @param pd_nested A nested parse table.
#' @param fun A transformer function that operates on a flat parse table.
#' @return A transformed, nested parse table. Whereas `fun` was applied to
#'   every level of nesting within `pd_nested`.
transform_nested <- function(pd_nested, fun) {
  if (is.null(pd_nested$child)) return()
  pd_nested <- fun(pd_nested)
  pd_nested$child <- map(pd_nested$child, transform_one)
  pd_nested
}
