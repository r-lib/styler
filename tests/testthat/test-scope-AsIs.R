test_that("no indention manipulation but spaces manipulation", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_spaces-",
    transformer = style_text, style = tidyverse_style, scope = I("spaces")
  ))
})

test_that("just indention", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_indention-",
    transformer = style_text,
    style = tidyverse_style, scope = I("indention")
  ))
})

test_that("indention and spaces", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_spaces_indention-",
    transformer = style_text,
    style = tidyverse_style, scope = I(c("indention", "spaces"))
  ))
})


test_that("line-break manipulation", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_line_breaks-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("line_breaks")
  ))
})


test_that("line-break manipulation", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_spaces_line_breaks-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("line_breaks", "spaces"))
  ))
})


test_that("tokens and indention", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_indention_tokens-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("tokens", "indention"))
  ))
})

test_that("tokens and indention", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_spaces_tokens-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I(c("spaces", "tokens"))
  ))
})

test_that("no manipulation at all", {
  expect_no_warning(test_collection(
    "scope-AsIs", "scope_none-",
    transformer = style_text,
    style = tidyverse_style,
    scope = I("none")
  ))
})
