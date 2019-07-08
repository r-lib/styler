run_test_impl <- function(path_executable,
                          path_candidate,
                          path_reference,
                          comparator = function(path_candidate, path_reference) {
                            testthat::expect_equivalent(
                              readLines(path_candidate),
                              readLines(path_reference)
                            )
                          },
                          error_msg) {
  tempdir <- tempdir()
  path_candidate_temp <- fs::path(tempdir, basename(path_candidate))
  fs::file_copy(path_candidate, tempdir, overwrite = TRUE)
  exec <- fs::path_abs(path_executable)
  path_stderr <- tempfile()
  withr::with_dir(
    fs::path_dir(path_candidate_temp),
    system2(exec, fs::path_file(path_candidate_temp), stderr = path_stderr)
  )
  if (is.null(error_msg)) {
    testthat::expect_equal(readLines(path_stderr), character(0))
  } else if (is.na(error_msg)) {
    comparator(path_candidate_temp, path_reference)
  } else {
    testthat::expect_match(
      paste(readLines(path_stderr), collapse = "\n"), error_msg,
      fixed = TRUE
    )
  }
}

run_test <- function(hook_name, file_name = hook_name, suffix = ".R",
                     comparator = function(path_candidate, path_reference) {
                       testthat::expect_equivalent(
                         readLines(path_candidate),
                         readLines(path_reference)
                       )
                     },
                     error_msg = NA) {
  path_executable <- file.path("bin", hook_name)
  test_ancestor <- file.path("tests", c("in", "out"), file_name)
  path_candidate <- paste0(test_ancestor, suffix)
  run_test_impl(path_executable, path_candidate[1], path_candidate[2], comparator, error_msg = error_msg)
}

run_test("styler-style-files")
run_test("usethis-use-tidy-description", "DESCRIPTION", "")
run_test(
  "custom-no-browser-statement",
  error_msg = "contains a `browser()` statement."
)
run_test(
  "custom-no-browser-statement",
  "custom-no-browser-statement-2",
  error_msg = NULL
)

run_test("custom-parsable-R", error_msg = "not parsable")

run_test("custom-parsable-R",
  "custom-parsable-R-2",
  error_msg = NULL
)
