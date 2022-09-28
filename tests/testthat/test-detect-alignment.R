test_that("does apply spacing rules only if not aligned", {
  skip_if_not_installed("tibble")
  library(tibble)

  expect_warning(test_collection("alignment",
    transformer = style_text
  ), NA)

  text <- "tribble(\n  ~x, ~y,\n  11, list(a = 1),\n  2, list(bjj = 2)\n)"
  expect_warning(style_text(text), NA)
})
