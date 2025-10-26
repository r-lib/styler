test_that("no indention manipulation but spaces manipulation", {
  expect_no_warning(test_collection(
    "scope-character", "scope_spaces",
    transformer = style_text, style = tidyverse_style, scope = "spaces"
  ))
})

test_that("no line-break manipulation", {
  expect_no_warning(test_collection(
    "scope-character", "scope_indention",
    transformer = style_text,
    style = tidyverse_style, scope = "indention"
  ))
})


test_that("no token manipulation", {
  expect_no_warning(test_collection(
    "scope-character", "scope_line_breaks",
    transformer = style_text,
    style = tidyverse_style,
    scope = "line_breaks"
  ))
})

test_that("no space manipulation", {
  expect_no_warning(test_collection(
    "scope-character", "scope_tokens",
    transformer = style_text,
    style = tidyverse_style,
    scope = "tokens"
  ))
})


test_that("no manipulation at all", {
  expect_no_warning(test_collection(
    "scope-character", "scope_none",
    transformer = style_text,
    style = tidyverse_style,
    scope = "none"
  ))
})
