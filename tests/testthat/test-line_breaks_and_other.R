test_that("line breaks involing curly brackets", {
  expect_no_warning(test_collection("line_breaks_and_other", "curly",
    transformer = style_text
  ))
})

test_that("line breaks involing curly brackets", {
  expect_no_warning(test_collection("line_breaks_and_other", "braces-fun-calls",
    transformer = style_text
  ))
})


test_that("line breaks involing curly brackets", {
  expect_no_warning(test_collection("line_breaks_and_other", "edge_comment_and_curly",
    transformer = style_text
  ))
})

test_that("adding and removing line breaks", {
  expect_no_warning(test_collection("line_breaks_and_other", "if",
    transformer = style_text
  ))
})

test_that("no line break after %>% if next token is comment", {
  expect_no_warning(test_collection("line_breaks_and_other", "pipe_and",
    transformer = style_text
  ))
})


test_that("line break before comma is removed and placed after comma ", {
  expect_no_warning(test_collection("line_breaks_and_other", "comma",
    transformer = style_text
  ))
})

test_that("line break before comma is removed and placed after comma ", {
  expect_no_warning(test_collection("line_breaks_and_other", "pipe-line",
    transformer = style_text
  ))
})

test_that("Can handle base R pie", {
  skip_if(getRversion() < "4.1")
  expect_no_warning(test_collection("line_breaks_and_other", "base-pipe-line",
    transformer = style_text
  ))
})

test_that("line break added for ggplot2 call", {
  expect_no_warning(test_collection("line_breaks_and_other", "ggplot2",
    transformer = style_text
  ))
})

test_that("drop redundant line breaks in assignments", {
  expect_no_warning(test_collection("line_breaks_and_other", "assignment",
    transformer = style_text, scope = I(c("line_breaks", "tokens"))
  ))
})

test_that("line is correctly broken around = ", {
  expect_no_warning(test_collection("line_breaks_and_other", "around-eq-sub",
    transformer = style_text
  ))
})

test_that("comments are not moved down after {", {
  expect_no_warning(test_collection("line_breaks_and_other", "comment-around-curly",
    transformer = style_text
  ))
})
