test_that("curly braces", {
  expect_no_warning(test_collection(
    "spacing", "round",
    transformer = style_text
  ))
})

test_that(":, ::, and :::", {
  expect_no_warning(test_collection(
    "spacing", "colon",
    transformer = style_text
  ))
})


test_that("comments and strict = FALSE", {
  expect_no_warning(test_collection(
    "spacing", "comments",
    transformer = style_text, stric = FALSE
  ))
})

test_that("Space placed after 'if' and before '('", {
  expect_no_warning(test_collection(
    "spacing", "spacing_if",
    transformer = style_text
  ))
})

test_that("space before comma is removed", {
  expect_no_warning(test_collection(
    "spacing", "spacing_comma",
    transformer = style_text
  ))
})


test_that("two commas are separated by a space", {
  expect_no_warning(test_collection(
    "spacing", "spacing_comma2",
    transformer = style_text
  ))
})

test_that("spacing between ! and bang is perserved", {
  expect_no_warning(test_collection(
    "spacing", "bang_bang_spacing",
    transformer = style_text
  ))
})

test_that("spacing around in works", {
  expect_no_warning(test_collection(
    "spacing", "spacing_in",
    transformer = style_text
  ))
})

test_that("no spaces after token FUNCTION", {
  expect_no_warning(test_collection(
    "spacing", "spacing_function",
    transformer = style_text, strict = FALSE
  ))
})

test_that("spacing around tilde", {
  expect_no_warning(test_collection(
    "spacing", "spacing-tilde",
    transformer = style_text, strict = TRUE
  ))
})

test_that("spacing around square brackets / braces", {
  expect_no_warning(test_collection(
    "spacing", "spacing-square",
    transformer = style_text, strict = TRUE
  ))
})

test_that("spacing around dollar", {
  expect_no_warning(test_collection(
    "spacing", "dollar",
    transformer = style_text, strict = TRUE
  ))
})
