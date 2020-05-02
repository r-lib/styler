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
#' @param suffix The suffix of `file_name`.
#' @inheritParams run_test_impl
#' @keywords internal
run_test <- function(hook_name,
                     file_name = hook_name,
                     suffix = ".R",
                     error_msg = NULL,
                     cmd_args = NULL,
                     copy = NULL) {
  path_executable <- system.file(
    fs::path("bin", hook_name),
    package = "precommit"
  )
  test_ancestor <- testthat::test_path("in", file_name)
  path_candidate <- paste0(test_ancestor, suffix)
  run_test_impl(
    path_executable, path_candidate[1],
    error_msg = error_msg,
    cmd_args = cmd_args,
    copy = copy
  )
}

#' Implement a test run
#'
#' @param path_executable The path to the executable bash script.
#' @param path_candidate The path to a file that should be modified by the
#'   executable.
#' @param copy Path with files to copy to the temp directory where the test
#'   is run. If the target destination relative to the temp dir where the hook
#'   is tested is not identical to the path from where a file should be copied,
#'   you can pass a named vector. The name is the target directory relative to
#'   the temp directory where the hook is executed (the temp directory will be
#'   the working directory at that time) and the value is the path that points
#'   to the place where the artifact is currently stored.
#' @param error_msg An expected error message. If no error is expected, this
#'   can be `NULL`. In that case, the `comparator` is applied.
#' @param cmd_args More arguments passed to the file. Pre-commit handles it as
#'   described [here](https://pre-commit.com/#arguments-pattern-in-hooks).
#' @keywords internal
run_test_impl <- function(path_executable,
                          path_candidate,
                          error_msg,
                          cmd_args,
                          copy) {
  expect_success <- is.null(error_msg)
  tempdir <- tempdir()
  if (!is.null(copy)) {
    if (is.null(names(copy))) {
      # no names, take basename
      new_dirs <- fs::path(tempdir, fs::path_dir(copy))
      fs::dir_create(new_dirs)
      paths_copy <- fs::path(new_dirs, fs::path_file(copy))
    } else {
      paths_copy <- fs::path(tempdir, names(copy))
      new_dirs <- fs::path_dir(paths_copy)
      fs::dir_create(new_dirs)
    }
    on.exit(fs::file_delete(paths_copy))
    fs::file_copy(copy, paths_copy, overwrite = TRUE)
  }
  path_candidate_temp <- fs::path(tempdir, basename(path_candidate))
  fs::file_copy(path_candidate, tempdir, overwrite = TRUE)
  path_stderr <- tempfile()
  status <- withr::with_dir(
    fs::path_dir(path_candidate_temp),
    {
      # https://r.789695.n4.nabble.com/Error-message-Rscript-should-not-be-used-without-a-path-td4748071.html
      system2(paste0(Sys.getenv("R_HOME"), "/bin/Rscript"),
        args = c(path_executable, cmd_args, fs::path_file(path_candidate_temp)),
        stderr = path_stderr
      )
    }
  )
  candidate <- readLines(path_candidate_temp)
  reference <- readLines(path_candidate)
  if (expect_success) {
    # file not changed + no stderr
    contents <- readLines(path_stderr)
    if (status != 0) {
      testthat::fail("Expected: No error. Found:", contents)
    }
    testthat::expect_equivalent(candidate, reference)
  } else if (!expect_success) {
    # either file changed or stderr
    if (is.na(error_msg)) {
      if (identical(candidate, reference)) {
        testthat::fail(paste0(
          path_candidate, " and ", path_candidate_temp,
          " are not supposed to be identical but they are"
        ))
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

on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}
