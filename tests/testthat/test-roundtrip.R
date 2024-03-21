


test_that("parse_tree_must_be_identical works", {
  expect_true(
    parse_tree_must_be_identical(tidyverse_style(scope = "line_breaks"))
  )
  expect_true(parse_tree_must_be_identical(tidyverse_style(scope = "spaces")))
  expect_true(
    parse_tree_must_be_identical(tidyverse_style(scope = "indention"))
  )
  expect_false(parse_tree_must_be_identical(tidyverse_style(scope = "tokens")))
})

test_that("correct styling does not give an error", {
  expect_snapshot({
    verify_roundtrip("1+1", "1 + 1")
  })
})

test_that("corrupt styling does give an error", {
  expect_snapshot_error(verify_roundtrip("1-1", "1 + 1"))
})


test_that("the output is asserted to be parsable", {
  expect_error(
    verify_roundtrip("1+1", "1 +) 1", parsable_only = TRUE),
    "Styling resulted in code that isn't parsable."
  )

  expect_silent(
    verify_roundtrip("1+1", "1 + 1", parsable_only = TRUE)
  )
})
