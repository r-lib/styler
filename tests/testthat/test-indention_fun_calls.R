context("test-indention_fun_calls.R")

test_that("edge cases work", {
  expect_warning(test_collection("indention_fun_calls",
    transformer = style_text, strict = FALSE
  ), NA)
})
