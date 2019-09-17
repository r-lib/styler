context("linebreaking added / removed correctly")

test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "curly",
                  transformer = style_text), NA)
})

test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "braces-fun-calls",
                                 transformer = style_text), NA)
})


test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "edge_comment_and_curly",
                                 transformer = style_text), NA)
})

test_that("adding and removing line breaks", {
  expect_warning(test_collection("line_breaks_and_other", "if",
                  transformer = style_text), NA)
})

test_that("no line break after %>% if next token is comment", {
  expect_warning(test_collection("line_breaks_and_other", "pipe_and",
                                 transformer = style_text), NA)
})


test_that("line break before comma is removed and placed after comma ", {
  expect_warning(test_collection("line_breaks_and_other", "comma",
                                 transformer = style_text), NA)
})

test_that("line break before comma is removed and placed after comma ", {
  expect_warning(test_collection("line_breaks_and_other", "pipe-line",
                                 transformer = style_text), NA)
})
