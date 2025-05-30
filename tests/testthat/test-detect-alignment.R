test_that("does apply spacing rules only if not aligned", {
  expect_no_warning(test_collection("alignment",
    transformer = style_text
  ))

  text <- "tribble(\n  ~x, ~y,\n  11, list(a = 1),\n  2, list(bjj = 2)\n)"
  expect_no_warning(style_text(text))
})
