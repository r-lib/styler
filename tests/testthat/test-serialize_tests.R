context("test testing functions")

test_that("No files to compare returns error", {
  expect_error(test_collection(
    "serialize_tests", "xyz",
    transformer = as_is
  ), "no items")
})

test_that("Can handle multiple in for one out file", {
  expect_warning(
    test_collection(
      "serialize_tests", "k2",
      transformer = identity
    ),
    c("k2\\-another\\-in_file.*k2\\-out")
  )

  expect_warning(
    test_collection(
      "serialize_tests", "k2",
      transformer = identity
    ),
    c("k2\\-in.*k2\\-out")
  )
})


test_that("properly detects non-match", {
  expect_warning(
    test_collection(
      "serialize_tests", "k3",
      transformer = identity,
      write_back = FALSE
    ),
    "different"
  )
})

test_that("properly detects match", {
  expect_message(
    test_collection(
      "serialize_tests", "correct",
      transformer = identity
    ),
    "identical"
  )
})
