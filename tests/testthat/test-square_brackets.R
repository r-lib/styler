test_that("square brackets cause indention", {
  expect_no_warning(test_collection(
    "indention_square_brackets",
    transformer = style_text
  ))
})
