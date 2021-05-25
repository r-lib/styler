context("linebreaking added / removed correctly")

test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "curly",
    transformer = style_text
  ), NA)
})

test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "braces-fun-calls",
    transformer = style_text
  ), NA)
})


test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "edge_comment_and_curly",
    transformer = style_text
  ), NA)
})

test_that("adding and removing line breaks", {
  expect_warning(test_collection("line_breaks_and_other", "if",
    transformer = style_text
  ), NA)
})

test_that("no line break after %>% if next token is comment", {
  expect_warning(test_collection("line_breaks_and_other", "pipe_and",
    transformer = style_text
  ), NA)
})


test_that("line break before comma is removed and placed after comma ", {
  expect_warning(test_collection("line_breaks_and_other", "comma",
    transformer = style_text
  ), NA)
})

test_that("line break before comma is removed and placed after comma ", {
  expect_warning(test_collection("line_breaks_and_other", "pipe-line",
    transformer = style_text
  ), NA)
})

test_that("Can handle base R pie", {
  skip_if(getRversion() < 4.1)
  expect_warning(test_collection("line_breaks_and_other", "base-pipe-line",
    transformer = style_text
  ), NA)
})

test_that("line break added for ggplot2 call", {
  expect_warning(test_collection("line_breaks_and_other", "ggplot2",
    transformer = style_text
  ), NA)
})

test_that("drop redundant line breaks in assignments", {
  expect_warning(test_collection("line_breaks_and_other", "assignment",
    transformer = style_text, scope = I(c("line_breaks", "tokens"))
  ), NA)
})
