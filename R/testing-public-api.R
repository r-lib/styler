#' Capture and post-process the output of `style_file` without causing side
#' effects
#'
#' @param file_in A vector passed to [testthat_file()] to construct the path
#'   to the reference file.
#' @return
#' A character vector with the captured output of [style_file()] called on
#' `file_in` ran in a temp dir to avoid side effects on the input file (because
#' the next time the test would ran, the file would not need styling). The
#' styling is carried out with a temporary working directory change to keep
#' filenames relative and avoid portability issues in the exact output
#' comparison which is needed when the system that runs the unit testing (CI)
#' is a different system than the one that created the reference value.
#' This also implies that the ruler width, which depend on the path
#' length, will again have the same width on all systems and is independent of
#' how many characters the path of the temporary directory has.
#' @importFrom utils capture.output
#' @keywords internal
catch_style_file_output <- function(file_in = c(
                                      "public-api",
                                      "xyzdir-dirty",
                                      "dirty-sample-with-scope-tokens.R"
                                    ),
                                    encoding) {
  file_in <- do.call(testthat_file, as.list(file_in))
  temp_path <- copy_to_tempdir(file_in)
  raw_output <- withr::with_dir(
    dirname(temp_path),
    capture.output(
      style_file(basename(temp_path), scope = "tokens")
    )
  )
  unlink(dirname(temp_path))
  raw_output
}

ls_testable_encodings <- function() {
  c("non-utf8", if (cli::is_utf8_output()) "utf8")
}

#' Test the dry argument
#' @param path A path to pass to the `styler`.
#' @param styler A function that takes `path`, typically a user exposed styler
#'   function that has side effects, like [style_file()].
#' @keywords internal
test_dry <- function(path, styler, styled = FALSE) {
  before <- readLines(path)
  summary <- styler(path, dry = "on")
  checker <- ifelse(styled, testthat::expect_false, testthat::expect_true)
  checker(summary$changed)
  testthat::expect_true(identical(before, readLines(path)))

  if (styled) {
    testthat::expect_error(styler(path, dry = "fail"), NA)
  } else {
    testthat::expect_error(styler(path, dry = "fail"), "would be modified")
  }
  testthat::expect_error(styler(path, dry = "other option"), "one of")
}
