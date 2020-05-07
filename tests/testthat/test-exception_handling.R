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

test_that("warning is given when transformers does not contain a version", {
  sg <- create_style_guide(style_guide_version = NULL)
  if (packageVersion("styler") < "1.4") {
    expect_fun <- expect_warning
  } else {
    expect_fun <- expect_error
  }
  expect_fun(
    assert_transformers(sg),
    "name and a version field are depreciated and won't be supported in styler >= 1.4"
  )
})
