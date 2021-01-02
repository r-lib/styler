context("scope AsIs")

test_that("no indention manipulation but spaces manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_spaces",
    transformer = style_text, style = tidyverse_style, scope = I("spaces")
  ), NA)
})

test_that("no line-break manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_indention",
    transformer = style_text,
    style = tidyverse_style, scope = I("indention")
  ), NA)
})


test_that("no token manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_line_breaks",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("line_breaks")
  ), NA)
})

test_that("no space manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_tokens",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("tokens")
  ), NA)
})


test_that("no manipulation at all", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_none",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("none")
  ), NA)
})
