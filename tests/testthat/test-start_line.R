context("start token")

test_that("leading spaces are preseved at start of text", {
  expect_warning(test_collection("start_line",
                  transformer = style_text, write_back= FALSE), NA)
})
