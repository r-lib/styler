run_test_impl <- function(path_executable, path_candidate, path_reference) {
  tempdir <- tempdir()
  path_candidate_temp <- fs::path(tempdir, basename(path_candidate))
  fs::file_copy(path_candidate, tempdir, overwrite = TRUE)
  exec <- fs::path_abs(path_executable)
  withr::with_dir(
    fs::path_dir(path_candidate_temp),
    system2(exec, fs::path_file(path_candidate_temp))
  )
  testthat::expect_equivalent(
    readLines(path_candidate_temp),
    readLines(path_reference)
  )
}

run_test <- function(hook_name, file_name = hook_name, suffix = ".R") {
  path_executable <- file.path("bin", hook_name)
  test_ancestor <- file.path("tests", c("in", "out"), file_name)
  path_candidate <- paste0(test_ancestor, suffix)
  run_test_impl(path_executable, path_candidate[1], path_candidate[2])
}

run_test("styler-style-files")
run_test("usethis-use-tidy-description", "DESCRIPTION", "")
