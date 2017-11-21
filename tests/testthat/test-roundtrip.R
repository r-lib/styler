context("roundtrip works")

test_that("correct styling does not give an error for scope < tokens", {
  expect_error(verify_roundtrip(
    "1+1", "1 + 1", tidyverse_style(scope = "spaces")
  ), NA)
  expect_error(verify_roundtrip(
    "1+1", "1 + 1", tidyverse_style(scope = "line_breaks")
  ), NA)
  expect_error(verify_roundtrip(
    "1+1", "10+10", tidyverse_style(scope = "tokens")
  ), NA)
})

test_that("incorrect styling does give an error", {
  expect_error(verify_roundtrip(
    "1-1", "1 + 1", tidyverse_style(scope = "spaces")
  ), "bug")
  expect_error(verify_roundtrip(
    "1-1", "1 + 1", tidyverse_style(scope = "line_breaks")
  ), "bug")
  expect_error(verify_roundtrip(
    "1-1", "1 + 1", tidyverse_style(scope = "tokens")
  ), NA)
})
