run_test_impl <- function(path_executable, path_candidate, path_reference) {
  tempdir <- tempdir()
  path_candidate_temp <- file.path(tempdir, basename(path_candidate))
  file.copy(path_candidate, tempdir)
  system2("./bin/styler-style-files", path_candidate_temp)
  testthat::expect_equivalent(
    readLines(path_candidate_temp), 
    readLines(path_reference)
  )
  
}

run_test <- function(hook_name) {
  path_executable <- file.path("bin", hook_name)
  test_ancestor <- file.path("tests", hook_name)
  path_candidate <- paste0(test_ancestor, "-in.R")
  path_reference <- paste0(test_ancestor, "-reference.R")
  browser()
  run_test_impl(path_executable, path_candidate, path_reference)
}

run_test("styler-style-files")
