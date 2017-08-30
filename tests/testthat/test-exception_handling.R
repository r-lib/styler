context("Exception handling")

test_that("style_text returns custom error", {
  expect_error(style_text("a <- 3 4"), "unexpected numeric constant")
})

test_that("style_file returns custom error", {
  expect_warning(
    style_file(testthat_file("exception_handling", "/parser-error.R")),
    "When processing"
  )
})
