context("test comment token insertion interaction")

test_that("token are added correctly to conditional statements", {
  expect_warning(test_collection(
      "insertion_comment_interaction", "just_if",
    transformer = style_text), NA)
})

test_that("token are added correctly to conditional statements", {
  expect_warning(test_collection(
    "insertion_comment_interaction", "if_else",
    transformer = style_text), NA)
})

test_that("token are added correctly to conditional statements", {
  expect_warning(test_collection(
    "insertion_comment_interaction", "if_else_if_else",
    transformer = style_text), NA)
})
