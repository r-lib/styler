#' Implement a test run
#'
#' @param path_executable The path to the executable bash script.
#' @param path_candidate The path to a file that should be modified by the
#'   executable.
run_test_impl <- function(path_executable,
                          path_candidate,
                          error_msg) {
  expect_success <- is.null(error_msg)
  tempdir <- tempdir()
  path_candidate_temp <- fs::path(tempdir, basename(path_candidate))
  fs::file_copy(path_candidate, tempdir, overwrite = TRUE)
  exec <- fs::path_abs(path_executable)
  path_stderr <- tempfile()
  withr::with_dir(
    fs::path_dir(path_candidate_temp),
    system2(exec, fs::path_file(path_candidate_temp), stderr = path_stderr)
  )
  candidate <- readLines(path_candidate_temp)
  reference <- readLines(path_candidate)
  if (expect_success) {
    # file not changed + no stderr
    contents <- readLines(path_stderr)
    if (!identical(contents, character(0))) {
      stop("Expected: No error. Found:", contents, call. = FALSE)
    }
    testthat::expect_equivalent(candidate, reference)
  } else if (!expect_success) {
    # either file changed or stderr
    if (is.na(error_msg)) {
      if (identical(candidate, reference)) {
        stop(
          path_candidate, " and ", path_reference,
          " are not supposed to be identical but they are"
        )
      }
    } else {
      contents <- readLines(path_stderr)
      testthat::expect_match(
        paste(contents, collapse = "\n"), error_msg,
        fixed = TRUE
      )
    }
  }
}

#' Run a test
#'
#' Tests for the executables used as pre-commit hooks via `entrypoint` in
#' `.pre-commit-config.yaml`.
#' @details
#' Two potential outcomes of a hooks are pass or fail. This is reflected on the
#' level of the executable: Fail means the executable fails or the file is
#' changed. Pass means the executable succeeds and the file is unchanged.
#' We check if the executable passes as follows:
#'
#' * If we expect success (by setting `error_msg` to `NULL`), we make sure
#'   nothing was written to sterr and the file content does not change.
#'
#' * If we expect failure, it can be due to changed file or due to failed
#'   executable. To check for failed executalble, we set `error_msg` to
#'   the message we expect. To check changed file content, we set `error_msg` to
#'   `NA`.
#'
#' @param hook_name The name of the hook in `bin/`.
#' @param file_name The file to test in `tests/in` (without extension).
#' @param suffix The fuffix of `file_name`.
#' @param error_msg An expected error message. If no error is expected, this
#'   can be `NULL`. In that case, the `comparator` is applied.
#' @param comparator A function thtat takes two paths as arguments: One to
#'   a file *after* the hooks has been applied and one to a reference file. Only
#'   used if `error_msg` is not `NULL`.
run_test <- function(hook_name, file_name = hook_name, suffix = ".R",
                     error_msg = NULL) {
  path_executable <- fs::path(here::here(), "bin", hook_name)
  test_ancestor <- fs::path(here::here(), "tests", "testthat", "in", file_name)
  path_candidate <- paste0(test_ancestor, suffix)
  run_test_impl(
    path_executable, path_candidate[1],
    error_msg = error_msg
  )
}
