#' Capture and post-process the output of `style_file` without causing side
#' effects
#'
#' @param file_in A vector passed to [testthat_file()] to construct the path
#'   to the reference file.
#' @return
#' A character vector with the captured output of [style_file()] called on
#' `file_in` ran in a temp dir to avoid side effects on the input file (because
#' the next time the test would ran, the file would not need styling).
#' In this output, the
#' path has been removed so only the file name is contained in the return
#' value to make the output portable across systems (in particular, to run it on
#' CI systems). Since the horizontal rules width depend on the length of the
#' path, the rules were also removed to standardize the ouptut of this function.
#' @keywords internal
catch_style_file_output <- function(file_in = c(
                                      "public-api",
                                      "xyzdir-dirty",
                                      "dirty-sample-with-scope-tokens.R"
                                    ),
                                    encoding) {
  temp_path <- copy_to_tempdir(do.call(testthat_file, as.list(file_in)))
  removed_path <- gsub(dirname(temp_path), "", capture.output(
    style_file(temp_path, scope = "tokens")
  ), fixed = TRUE)
  removed_rules <- gsub(
    ifelse(encoding == "utf8", "\u2500+", "-+"),
    "", removed_path
  )
  unlink(dirname(temp_path))
  removed_rules
}

ls_testable_encodings <- function() {
  c("non-utf8", if (cli::is_utf8_output()) "utf8")
}
