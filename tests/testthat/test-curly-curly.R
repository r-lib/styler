context("curly-curly")

test_that("curly-culry", {
  expect_warning(test_collection("curly-curly",
    "mixed",
    transformer = style_text,
    write_back = TRUE
  ), NA)
})
