context("Exception handling")

test_that("style_text returns custom error", {
  expect_error(style_text("a <- 3 4"), "unexpected numeric constant")
})


base <- rprojroot::find_testthat_root_file("exception_handling")
test_that("style_file returns custom error", {
  expect_warning(
    style_file(paste0(base, "/parser-error.R")),
    "When processing"
  )
})
