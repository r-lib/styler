#' Apply a function to the contents of a file
#'
#' Transforms a file with a function.
#' @inheritParams transform_utf8_one
#' @keywords internal
transform_utf8 <- function(path, fun, dry) {
  map_lgl(path, transform_utf8_one, fun = fun, dry = dry) %>%
    set_names(path)
}

#' Potentially transform a file
#'
#' @param path A vector with file paths to transform.
#' @param fun A function that returns a character vector.
#' @param dry To indicate whether styler should run in *dry* mode, i.e. refrain
#'   from writing back to files .`"on"` and `"fail"` both don't write back, the
#'   latter returns an error if the input code is not identical to the result
#'   of styling. "off", the default, writes back if the input and output of
#'   styling are not identical.
#' @keywords internal
transform_utf8_one <- function(path, fun, dry) {
  rlang::arg_match(dry, c("on", "off", "fail"))
  rlang::with_handlers(
    {
      file_with_info <- read_utf8(path)
      # only write back when changed OR when there was a missing newline
      new <- unclass(fun(file_with_info$text))
      if (identical(new, "")) {
        new <- character(0L)
      }
      identical_content <- identical(file_with_info$text, new)
      identical <- identical_content && !file_with_info$missing_EOF_line_break
      if (!identical) {
        if (dry == "fail") {
          rlang::abort(
            paste0(
              "File `", path, "` would be modified by styler and argument dry",
              " is set to 'fail'."
            ),
            class = "dryError"
          )
        } else if (dry == "on") {
          # don't do anything
        } else if (dry == "off") {
          write_utf8(new, path)
        } else {
          # not implemented
        }
      }
      !identical
    },
    error = function(e) {
      if (inherits(e, "dryError")) {
        rlang::abort(conditionMessage(e))
      } else {
        warn(paste0("When processing ", path, ": ", conditionMessage(e)))
      }
      NA
    }
  )
}

#' Read UTF-8
#'
#' Reads an UTF-8 file, returning the content and whether or not the final line
#' was blank. This information is required higher up in the call stack because
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
  if (is.character(out)) {
    list(
      text = out,
      missing_EOF_line_break = FALSE
    )
  } else if (inherits(out, "error")) {
    rlang::abort(out$message)
  } else if (inherits(out, "warning")) {
    list(
      text = read_utf8_bare(path, warn = FALSE),
      missing_EOF_line_break = grepl("incomplete", out$message, fixed = TRUE)
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
  if (n > 0L) {
    stop(
      c(
        "The file ", con, " is not encoded in UTF-8. ",
        "These lines contain invalid UTF-8 characters: "
      ),
      toString(c(utils::head(i), if (n > 6L) "..."))
    )
  }
  x
}

#' Drop-in replacement for `xfun:::invalid_utf8()`
#' @keywords internal
invalid_utf8 <- function(x) {
  which(!is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")))
}

#' Drop-in replacement for `xfun::write_utf8()`
#' @keywords internal
write_utf8 <- function(text, con, ...) {
  withr::local_options(encoding = "native.enc")
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}
