#' Transform code from R or Rmd files
#'
#' @inheritParams utf8::transform_lines_enc
#' @param ... Further arguments passed to `utf8::transform_lines_enc()`.
transform_code <- function(path, fun, verbose, ...) {
  if (grepl("\\.R$", path, ignore.case = TRUE)) {
    utf8::transform_lines_enc(path, fun = fun, ...)
  } else if (grepl("\\.Rmd$", path, ignore.case = TRUE)) {
    utf8::transform_lines_enc(path, fun = partial(transform_rmd, transformer_fun = fun), ...)
  } else {
    stop("")
  }
}

#' Transform Rmd contents with a transformer function
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


#' Identify R and text chunks within an Rmd
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
#' Determine a regex pattern for identifying R code chunks
#'
#' @inheritParams identify_chunks
get_knitr_pattern <- function(lines) {
  pattern <- knitr:::detect_pattern(lines, "rmd")
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}
