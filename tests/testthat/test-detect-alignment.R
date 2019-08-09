test_that("does apply spacing rules only if not aligned", {
  expect_warning(test_collection("alignment",
    transformer = style_text
  ), NA)
})
