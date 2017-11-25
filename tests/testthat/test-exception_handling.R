context("Exception handling")

test_that("style_text returns custom error", {
  expect_error(style_text("a <- 3 4"), "unexpected numeric constant")
})

test_that("style_file returns custom error", {
  capture_output(expect_warning(
    style_file(testthat_file("exception_handling", "parser-error.R")),
    "When processing"
  ))
})


test_that("style_text with no tokens returns empty string and warning", {
  expect_warning(style_text("\n\n"), "not contain any tokens.")
})

test_that("style_file with no tokens returns empty string and warning", {
  capture_output(expect_warning(
    style_file(testthat_file("exception_handling", "empty_file.R")),
    "not contain any tokens."
  ))
})
