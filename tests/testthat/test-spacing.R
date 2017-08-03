context("spacing")

test_that("curly braces", {
  expect_warning(test_collection(
    "spacing", "round",
    transformer = style_text), NA)
})
