context("start token")

test_that("leading spaces are preserved at start of text", {
  expect_warning(test_collection("start_line",
    transformer = style_empty, write_back = TRUE
  ), NA)
})
