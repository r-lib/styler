context("styler")

test_that("can style example source file", {
  text <- style_text(utf8::read_lines_enc("example-in.txt"))
  expect_false(
    utf8::transform_lines_enc("example-out.txt", function(x) text))
})
