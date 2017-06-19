# library("testthat")
# devtools::load_all()
context("indention after operators")

test_that("operator only are indented correctly", {
  expect_warning(test_collection("indention_operators",
                                 "operators_only",
                                 transformer = style_text), NA)
})

test_that("operator and round braces are indented correctly", {
  expect_warning(test_collection("indention_operators",
                                 "operators_and_round_only",
                                 transformer = style_text), NA)
})


test_that("operator and round braces and signs are indented correctly", {
  expect_warning(test_collection("indention_operators",
                                 "operators_and_round_and_sign",
                                 transformer = style_text), NA)
})

test_that("operator with multilines are indented correctly", {
  expect_warning(test_collection("indention_operators",
                                 "operators_multi_line_only",
                                 transformer = style_text), NA)
})
