context("spacing")

test_that("curly braces", {
  expect_warning(test_collection(
    "spacing", "round",
    transformer = style_text), NA)
})

test_that(":, ::, and :::", {
  expect_warning(test_collection(
    "spacing", "colon",
    transformer = style_text), NA)
})


test_that("comments and strict = FALSE", {
  expect_warning(test_collection(
    "spacing", "comments",
    transformer = style_text, stric = FALSE), NA)
})
