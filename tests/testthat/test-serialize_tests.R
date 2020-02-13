context("test testing functions")

test_that("No files to compare returns error", {
  expect_error(test_collection("serialize_tests", "xyz",
    transformer = as_is
  ), "no items")
})

test_that("properly detects non-match", {
  expect_warning(
    test_collection("serialize_tests", "k3",
      transformer = identity,
      write_back = FALSE
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
