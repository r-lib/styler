context("styler")

test_that("can style example source file", {
  text <- style_text(utf8::read_lines_enc("example/in.R"))
  expect_false(
    utf8::transform_lines_enc("example/out.R", function(x) text))
})

test_that("can style example source file (relaxed)", {
  text <- style_text(utf8::read_lines_enc("example/in.R"), get_transformers(strict = FALSE))
  expect_false(
    utf8::transform_lines_enc("example/out-relaxed.R", function(x) text))
})

test_that("removes space at EOL", {
  expect_equal(style_text("a() "), "a()")
  expect_equal(style_text("a()  # comment "), "a()  # comment")
})

test_that("removes blank lines at EOF", {
  expect_equal(style_text(c("a() ", "", "")), "a()")
})
