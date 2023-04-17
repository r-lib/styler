#' Transform code from R, Rmd or Rnw files
#'
#' A wrapper which initiates the styling of
#' either R, Rmd or Rnw files by passing the relevant transformer function for
#' each case.
#'
#' @inheritParams transform_utf8
#' @param ... Further arguments passed to [transform_utf8()].

#' @keywords internal
transform_code <- function(path, fun, ..., dry) {
  if (is_plain_r_file(path) || is_rprofile_file(path)) {
    transform_utf8(path, fun = fun, ..., dry = dry)
  } else if (is_rmd_file(path) || is_qmd_file(path)) {
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
#' an Rmd or Rnw file and recombines the resulting (styled) code chunks with the
#' text chunks.
#'
#' @param transformer_fun A styler transformer function.
#' @inheritParams separate_chunks
#' @keywords internal
transform_mixed <- function(lines, transformer_fun, filetype) {
  chunks <- separate_chunks(lines, filetype)
  chunks$r_chunks <- map(chunks$r_chunks, transform_mixed_non_empty,
    transformer_fun = transformer_fun
  )
  map2(chunks$text_chunks, c(chunks$r_chunks, list(character(0L))), c) %>%
    flatten_chr()
}

#' Ensure for `.Rmd` and friends that a code chunk without code is formatted as
#' a code chunk without any lines.
#' @keywords internal
transform_mixed_non_empty <- function(r_chunk, transformer_fun) {
  trimmed <- trimws(r_chunk)
  if (all(trimmed == "") || identical(trimmed, character(0L))) {
    character(0L)
  } else {
    transformer_fun(r_chunk)
  }
}

#' Separate chunks within Rmd and Rnw contents
#'
#' Identifies and separates the code and text chunks (the latter includes non-R
#' code) within an Rmd or Rnw file, and returns these separately.
#' @param lines A character vector of lines from an Rmd or Rnw file.
#' @param filetype A string indicating the filetype - either 'Rmd' or 'Rnw'.
#' @keywords internal
separate_chunks <- function(lines, filetype) {
  r_raw_chunks <- identify_raw_chunks(lines, filetype = filetype)

  r_chunks <- map2(
    r_raw_chunks$starts, r_raw_chunks$ends, ~ lines[seq2(.x + 1L, .y - 1L)]
  )

  text_chunks <- map2(
    c(1L, r_raw_chunks$ends), c(r_raw_chunks$starts, length(lines)),
    ~ lines[seq2(.x, .y)]
  )
  list(r_chunks = r_chunks, text_chunks = text_chunks)
}

#' Identifies raw Rmd or Rnw code chunks
#'
#' Raw in the sense that these chunks don't contain pure R code, but they
#' contain a header and footer of markdown. Only code chunks that have an engine
#' whose name matches `engine-pattern` are considered as R code.
#' For every opening, we match the next closing. If there are not the same
#' amount of closing and openings after this matching, we throw an error.
#' Similarly, if there are two openings before a closing, the closing gets
#' matched twice, on which we throw an error.
#' @inheritParams separate_chunks
#' @param engine_pattern A regular expression that must match the engine name.

#' @keywords internal
identify_raw_chunks <- function(lines,
                                filetype,
                                engine_pattern = get_engine_pattern()) {
  pattern <- get_knitr_pattern(filetype)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    abort("Unrecognized chunk pattern!")
  }

  if (filetype == "Rmd") {
    starts <- grep(
      "^[\t >]*```+\\s*\\{([Rr]( *[ ,].*)?)\\}\\s*$", lines,
      perl = TRUE
    )
    ends <- grep("^[\t >]*```+\\s*$", lines, perl = TRUE)
    ends <- purrr::imap_int(starts, ~ ends[which(ends > .x)[1L]]) %>%
      stats::na.omit()
    if (length(starts) != length(ends) || anyDuplicated(ends) != 0L) {
      abort("Malformed file!")
    }
  } else if (filetype == "Rnw") {
    starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
    ends <- grep(pattern$chunk.end, lines, perl = TRUE)
    if (length(starts) != length(ends)) {
      abort("Malformed file!")
    }
  }

  purrr::map2(starts, ends, finalize_raw_chunks,
    filetype = filetype, lines = lines
  ) %>%
    purrr::compact() %>%
    purrr::transpose()
}

#' Drop start / stop, when formatting is turned off
#'
#' If `tidy = FALSE` (the knitr code chunk default), code is not styled upon
#' knitting. If it is explicitly added to a code chunk, the code chunk is in
#' addition not styled with styler when formatting the document.
#' @keywords internal
finalize_raw_chunks <- function(start, end, filetype, lines) {
  header <- gsub(get_knitr_pattern(filetype)$chunk.begin, "\\2", lines[start])
  # matches last , tidy = TRUE, ignoring quotes!
  extracted_false <- gsub(
    ".*,\\s*\\t*tidy\\s*\\t*=\\s*\\t*(F|FALSE)(\\s+.*|\\t+.*|,+.*|)$",
    "\\1",
    header
  )
  if (extracted_false %in% c("F", "FALSE")) {
    NULL
  } else {
    list(starts = start, ends = end)
  }
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
