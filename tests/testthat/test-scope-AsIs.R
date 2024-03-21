

test_that("no indention manipulation but spaces manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_spaces-",
    transformer = style_text, style = tidyverse_style, scope = I("spaces")
  ), NA)
})

test_that("just indention", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_indention-",
    transformer = style_text,
    style = tidyverse_style, scope = I("indention")
  ), NA)
})

test_that("indention and spaces", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_spaces_indention-",
    transformer = style_text,
    style = tidyverse_style, scope = I(c("indention", "spaces"))
  ), NA)
})


test_that("line-break manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_line_breaks-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("line_breaks")
  ), NA)
})


test_that("line-break manipulation", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_spaces_line_breaks-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("line_breaks", "spaces"))
  ), NA)
})


test_that("tokens and indention", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_indention_tokens-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("tokens", "indention"))
  ), NA)
})

test_that("tokens and indention", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_spaces_tokens-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("spaces", "tokens"))
  ), NA)
})

test_that("no manipulation at all", {
  expect_warning(test_collection(
    "scope-AsIs", "scope_none-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("none")
  ), NA)
})
