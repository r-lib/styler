

test_that("No files to compare returns error", {
  expect_error(test_collection("serialize_tests", "xyz",
    transformer = as_is
  ), "no items")
})

test_that("properly detects non-match", {
  path_out <- test_path("serialize_tests", "k3-out.R")
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

test_that("detects non-matching style guides", {
  sg <- create_style_guide(
    space = list(
      a1 = function(...) NULL,
      b1 = function(...) 1
    ),
    transformers_drop = specify_transformers_drop(
      spaces = c(a1 = "'+'")
    )
  )
  expect_silent(test_transformers_drop(sg))

  sg <- create_style_guide(
    space = list(
      a1 = function(...) NULL
    ),
    transformers_drop = specify_transformers_drop(
      spaces = c(a2 = "'+'")
    )
  )
  expect_error(test_transformers_drop(sg))
})
