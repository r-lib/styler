test_that("curly-culry", {
  expect_no_warning(test_collection("curly-curly",
    "mixed",
    transformer = style_text
  ))
})
