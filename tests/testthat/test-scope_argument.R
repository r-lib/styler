context("scope argument")

test_that("no indention manipulation but spaces manipulation", {
  expect_warning(test_collection(
    "scope_argument", "scope_spaces",
    transformer = style_text, style = tidyverse_style, scope = "spaces"
  ), NA)
})

test_that("no line-break manipulation", {
  expect_warning(test_collection(
    "scope_argument", "scope_indention",
    transformer = style_text,
    style = tidyverse_style, scope = "indention"
  ), NA)
})


test_that("no token manipulation", {
  expect_warning(test_collection(
    "scope_argument", "scope_line_breaks",
    transformer = style_text,
    style = tidyverse_style,
    scope = "line_breaks"
  ), NA)
})

test_that("no space manipulation", {
  expect_warning(test_collection(
    "scope_argument", "scope_tokens",
    transformer = style_text,
    style = tidyverse_style,
    scope = "tokens"
  ), NA)
})


test_that("no manipulation at all", {
  expect_warning(test_collection(
    "scope_argument", "scope_none",
    transformer = style_text,
    style = tidyverse_style,
    scope = "none"
  ), NA)
})
