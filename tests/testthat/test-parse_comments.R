library("testthat")
context("correctly treats comments")


test_that("comments are treated corectly", {
  expect_warning(test_collection("parse_comments",
                                 "mixed",
                                 transformer = style_empty), NA)

  expect_warning(test_collection("parse_comments",
                                 "just_comments",
                                 transformer = style_empty), NA)


  expect_warning(test_collection("parse_comments",
                                 "with_indention",
                                 transformer = style_text), NA)

  # top-level test with indention
})
