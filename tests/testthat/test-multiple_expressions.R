library("testthat")


test_that("simple multiple expressions are styled correctly", {
  expect_no_warning(test_collection("multiple_expressions",
    "two_simple",
    transformer = style_text
  ))
})

test_that("complex multiple expressions are styled correctly", {
  expect_no_warning(test_collection("multiple_expressions",
    "three_complex",
    transformer = style_text
  ))
})
