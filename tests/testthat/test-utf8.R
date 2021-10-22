context("UTF-6 with 4.2")

# TODO: change this test once R 4.2 is released

# for more, see: https://github.com/r-lib/styler/issues/847
# this behavior is likely to change with R 4.2
test_that("styling unicode characters on Windows", {
  skip_if_not(.Platform$OS.type[[1]] == "windows")

  expect_identical(
    styler::style_text('suit <- "♠"'),
    structure("suit <- \"<U+2660>\"", class = "vertical")
  )
})
