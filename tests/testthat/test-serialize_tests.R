context("test testing functions")

test_that("No files to compare returns error", {
  expect_error(test_collection("serialize_tests", "xyz",
    transformer = as_is
  ), "no items")
})

test_that("properly detects non-match", {
  path_out <- test_path('serialize_tests', 'k3-out.R')
  before <- readLines(path_out)
  withr::defer(writeLines(before, path_out))
  expect_warning(
    test_collection("serialize_tests", "k3",
      transformer = identity
    ),
    "different"
  )
})

test_that("properly detects match", {
  expect_message(
    test_collection("serialize_tests", "correct",
      transformer = identity
    ),
    "identical"
  )
})
