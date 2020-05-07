#' Transform code from R, Rmd or Rnw files
#'
#' A wrapper which initiates the styling of
#' either R, Rmd or Rnw files by passing the relevant transformer function for each
#' case.
#'
#' @inheritParams transform_utf8
#' @param ... Further arguments passed to [transform_utf8()].
#' @importFrom rlang abort
#' @keywords internal
transform_code <- function(path, fun, ..., dry) {
  if (is_plain_r_file(path) || is_rprofile_file(path)) {
    transform_utf8(path, fun = fun, ..., dry = dry)
  } else if (is_rmd_file(path)) {
    transform_utf8(path,
      fun = partial(transform_mixed, transformer_fun = fun, filetype = "Rmd"),
      ..., dry = dry
    )
  } else if (is_rnw_file(path)) {
    transform_utf8(path,
      fun = partial(transform_mixed, transformer_fun = fun, filetype = "Rnw"),
      ..., dry = dry
    )
  } else {
    abort(paste(path, "is not an R, Rmd or Rnw file"))
  }
}

#' Transform mixed contents
#'
#' Applies the supplied transformer function to code chunks identified within
#' an Rmd or Rnw file and recombines the resulting (styled) code chunks with the text
#' chunks.
#'
#' @param transformer_fun A styler transformer function.
#' @inheritParams separate_chunks
#' @importFrom purrr flatten_chr
#' @keywords internal
transform_mixed <- function(lines, transformer_fun, filetype) {
  chunks <- separate_chunks(lines, filetype)
  chunks$r_chunks <- map(chunks$r_chunks, transformer_fun)

  map2(chunks$text_chunks, c(chunks$r_chunks, list(character(0))), c) %>%
    flatten_chr()
}

#' Separate chunks within Rmd and Rnw contents
#'
#' Identifies and separates the code and text chunks (the latter includes non-R
#' code) within an Rmd or Rnw file, and returns these separately.
#' @param lines A character vector of lines from an Rmd or Rnw file.
#' @param filetype A string indicating the filetype - either 'Rmd' or 'Rnw'.
#' @importFrom purrr map2
#' @importFrom rlang seq2
#' @keywords internal
separate_chunks <- function(lines, filetype) {
  r_raw_chunks <- identify_raw_chunks(lines, filetype = filetype)

  r_chunks <- map2(
    r_raw_chunks$starts, r_raw_chunks$ends, ~ lines[seq2(.x + 1, .y - 1)]
  )

  text_chunks <- map2(
    c(1, r_raw_chunks$ends), c(r_raw_chunks$starts, length(lines)),
    ~ lines[seq2(.x, .y)]
  )
  lst(r_chunks, text_chunks)
}

#' Identifies raw Rmd or Rnw code chunks
#'
#' Raw in the sense that these chunks don't contain pure R code, but they
#' contain a header and footer of markdown. Only code chunks that have an engine
#' whose name matches `engine-pattern` are considered as R code.
#' @inheritParams separate_chunks
#' @param engine_pattern A regular expression that must match the engine name.
#' @importFrom rlang abort
#' @keywords internal
identify_raw_chunks <- function(lines, filetype, engine_pattern = get_engine_pattern()) {
  pattern <- get_knitr_pattern(filetype)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    abort("Unrecognized chunk pattern!")
  }

  if (filetype == "Rmd") {
    chunks <- grep("^[\t >]*```+\\s*", lines, perl = TRUE)
    starts <- odd(chunks)
    ends <- even(chunks)
    is_r_code <- grepl(
      paste0("^[\t >]*```+\\s*\\{\\s*", engine_pattern, "[\\s\\},]"),
      lines[starts],
      perl = TRUE
    )
  } else if (filetype == "Rnw") {
    starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
    ends <- grep(pattern$chunk.end, lines, perl = TRUE)
    is_r_code <- rep(TRUE, length(starts))
  }

  if (length(starts) != length(ends)) {
    abort("Malformed file!")
  }

  list(starts = starts[is_r_code], ends = ends[is_r_code])
}

#' What's the engine pattern for rmd code chunks?
#'
#' The function returns the regular expression pattern that identifies
#' all r engines in Rmd chunks. Defaults to `[Rr]`. You probably only want to
#' change this if you create a knitr engine that processes R code but is not
#' the default engine `r`.
#' The pattern must be followed by a space (in the case the chunk is given
#' a name), a comma (if no name is given but further options are passed to the
#' engine) or a closing curly brace (in case no option and no name is given to
#' the chunk).
#' @keywords internal
get_engine_pattern <- function() {
  "[rR]"
}

#' Get chunk pattern
#'
#' Determine a regex pattern for identifying R code chunks.
#'
#' @inheritParams separate_chunks
#' @keywords internal
get_knitr_pattern <- function(filetype) {
  if (filetype == "Rnw") {
    knitr::all_patterns[["rnw"]]
  } else if (filetype == "Rmd") {
    knitr::all_patterns[["md"]]
  }
}
