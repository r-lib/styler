context("indention square brackets")

test_that("square brackets cause indention", {
  expect_warning(test_collection(
    "indention_square_brackets",
    "square_brackets_line_break",
    transformer = style_text
  ), NA)
})
