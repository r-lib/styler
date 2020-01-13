#' Apply a function to the contents of a file
#'
#' Transforms a file with a function.
#' @param path A vector with file paths to transform.
#' @param fun A function that returns a character vector.
#' @param write_back Whether or not the results of the transformation should
#'   be written back to the file.
#' @importFrom magrittr set_names
#' @importFrom rlang abort
#' @keywords internal
transform_utf8 <- function(path, fun, write_back = TRUE) {
  map_lgl(path, transform_utf8_one, fun = fun, write_back = write_back) %>%
    set_names(path)
}

#' @importFrom rlang with_handlers warn
transform_utf8_one <- function(path, fun, write_back) {
  with_handlers({
    file_with_info <- read_utf8(path)
    # only write back when changed OR when there was a missing newline
    new <- fun(file_with_info$text)
    identical_content <- identical(unclass(file_with_info$text), unclass(new))
    identical <- identical_content && !file_with_info$missing_EOF_line_break
    if (!identical && write_back) {
      xfun::write_utf8(new, path)
    }
    !identical
  }, error = function(e) {
    warn(paste0("When processing ", path, ": ", conditionMessage(e)))
    NA
  })
}

#' Read UTF-8
#'
#' Reads an UTF-8 file, returning the content and whether or not the final line
#' was blank. This information is required higher up in the callstack because
#' we should write back if contents changed or if there is no blank line at the
#' EOF. A perfectly styled file with no EOF blank line will gain such a line
#' with this implementation.
#' @param path A path to a file to read.
#' @keywords internal
read_utf8 <- function(path) {
  out <- rlang::with_handlers(
    read_utf8_bare(path),
    warning = function(w) w,
    error = function(e) e
  )
  if (inherits(out, "character")) {
    list(
      text = out,
      missing_EOF_line_break = FALSE
    )
  } else if (inherits(out, "error")) {
    rlang::abort(out$message)
  } else if (inherits(out, "warning")) {
    list(
      text = read_utf8_bare(path, warn = FALSE),
      missing_EOF_line_break = grepl("incomplete", out$message)
    )
  }
}

#' Drop-in replacement for `xfun::read_utf8()`, with an optional `warn`
#' argument.
#' @keywords internal
read_utf8_bare <- function(con, warn = TRUE) {
  x <- readLines(con, encoding = "UTF-8", warn = warn)
  i <- invalid_utf8(x)
  n <- length(i)
  if (n > 0) {
    stop(
      c(
        "The file ", con, " is not encoded in UTF-8. ",
        "These lines contain invalid UTF-8 characters: "
      ),
      paste(c(utils::head(i), if (n > 6) "..."), collapse = ", ")
    )
  }
  x
}

#' Drop-in replacement for `xfun:::invalid_utf8()`
#' @keywords internal
invalid_utf8 <- function(x) {
  which(!is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")))
}
