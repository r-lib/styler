context("roundtrip works")


test_that("can_verify_roundtrip works", {
  expect_true(can_verify_roundtrip(tidyverse_style(scope = "line_breaks")))
  expect_true(can_verify_roundtrip(tidyverse_style(scope = "spaces")))
  expect_true(can_verify_roundtrip(tidyverse_style(scope = "indention")))
  expect_false(can_verify_roundtrip(tidyverse_style(scope = "tokens")))
})

test_that("correct styling does not give an error", {
  expect_error(verify_roundtrip("1+1", "1 + 1"), NA)
})

test_that("corrupt styling does give an error", {
  expect_error(verify_roundtrip("1-1", "1 + 1"), "bug")
})
