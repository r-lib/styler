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
      verbose = verbose
    )
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
  chunks <- separate_chunks(lines)
  chunks$r_chunks <- map(chunks$r_chunks, transformer_fun)

  map2(chunks$text_chunks, c(chunks$r_chunks, list(character(0))), c) %>%
    flatten_chr()
}


#' Separate chunks within Rmd contents
#'
#' Identifies and separates the code and text chunks (the latter includes non-R
#' code) within an Rmd file, and returns these separately.
#' @param lines a character vector of lines from an Rmd file
#' @importFrom purrr map2
#' @importFrom rlang seq2
separate_chunks <- function(lines) {
  r_raw_chunks <- identify_r_raw_chunks(lines)
  r_chunks <- map2(
    r_raw_chunks$starts, r_raw_chunks$ends, ~lines[seq2(.x + 1, .y - 1)]
  )

  text_chunks <- map2(
    c(1, r_raw_chunks$ends), c(r_raw_chunks$starts, length(lines)),
    ~lines[seq2(.x, .y)]
  )
  lst(r_chunks, text_chunks)
}

#' Identifies raw R code chunks
#'
#' Raw in the sense that these chunks don't contain pure R code, but they
#' contain a header and footer of markdown. Only code chunks that have an engine
#' whose name matches `engine-pattern` are considered as R code.
#' @inheritParams separate_chunks
#' @param engine_pattern A regular expression that must match the engine name.
identify_r_raw_chunks <- function(lines, engine_pattern = "[rR]") {
  pattern <- get_knitr_pattern(lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    stop("Unrecognized chunk pattern!", call. = FALSE)
  }
  starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
  ends <- grep(pattern$chunk.end, lines, perl = TRUE)

  if (length(starts) != length(ends)) {
    stop("Malformed file!", call. = FALSE)
  }

  is_r_code <- grepl(
    paste0("^[\t >]*```+\\s*\\{(", engine_pattern, ".*)\\}\\s*$"),
    lines[starts], perl = TRUE
  )
  list(starts = starts[is_r_code], ends = ends[is_r_code])
}

#' Get chunk pattern
#'
#' Determine a regex pattern for identifying R code chunks.
#'
#' @inheritParams separate_chunks
get_knitr_pattern <- function(lines) {
  knitr::all_patterns[["md"]]
}
