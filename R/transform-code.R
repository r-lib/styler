#' Transform code from R or Rmd files
#'
#' A wrapper for [enc::transform_lines_enc()] which initiates the styling of
#' either R or Rmd files by passing the relevant transformer function for each
#' case.
#'
#' @inheritParams enc::transform_lines_enc
#' @param ... Further arguments passed to `enc::transform_lines_enc()`.
transform_code <- function(path, fun, verbose = FALSE, ...) {
  if (is_plain_r_file(path)) {
    enc::transform_lines_enc(path, fun = fun, ..., verbose = verbose)
  } else if (is_rmd_file(path)) {
    enc::transform_lines_enc(
      path, fun = partial(transform_rmd, transformer_fun = fun), ...,
      verbose = verbose)
  } else {
    stop(path, " is not an R or Rmd file")
  }
}

#' Transform Rmd contents
#'
#' Applies the supplied transformer function to code chunks identified within
#' an Rmd file and recombines the resulting (styled) code chunks with the text
#' chunks.
#'
#' @param lines A character vector of lines from an Rmd file
#' @param transformer_fun A styler transformer function
#' @importFrom purrr flatten_chr
transform_rmd <- function(lines, transformer_fun) {
  chunks <- identify_chunks(lines)
  chunks$r_chunks <- map(chunks$r_chunks, transformer_fun)

  map2(chunks$text_chunks, c(chunks$r_chunks, list(character(0))), c) %>%
    flatten_chr()
}


#' Identify chunks within Rmd contents
#'
#' Identifies the code and text chunks within an Rmd file, and returns these
#' as a nested list.
#'
#' @param lines a character vector of lines from an Rmd file
#'
#' @importFrom purrr map2
#' @importFrom rlang seq2
identify_chunks <- function(lines) {
  pattern <- get_knitr_pattern(lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    stop("Unrecognized chunk pattern!", call. = FALSE)
  }

  starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
  ends <- grep(pattern$chunk.end, lines, perl = TRUE)

  if (length(starts) != length(ends)) {
    stop("Malformed file!", call. = FALSE)
  }

  r_chunks <- map2(starts, ends, ~lines[seq2(.x + 1, .y - 1)])

  text_chunks <- map2(c(1, ends), c(starts, length(lines)), ~lines[seq2(.x, .y)])

  lst(r_chunks, text_chunks)
}

#' Get chunk pattern
#'
#' Determine a regex pattern for identifying R code chunks.
#'
#' @inheritParams identify_chunks
get_knitr_pattern <- function(lines) {
    knitr::all_patterns[["md"]]
}
