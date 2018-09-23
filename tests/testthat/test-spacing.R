context("spacing")

test_that("curly braces", {
  expect_warning(test_collection(
    "spacing", "round",
    transformer = style_text), NA)
})

test_that(":, ::, and :::", {
  expect_warning(test_collection(
    "spacing", "colon",
    transformer = style_text), NA)
})


test_that("comments and strict = FALSE", {
  expect_warning(test_collection(
    "spacing", "comments",
    transformer = style_text, stric = FALSE), NA)
})

test_that("Space placed after 'if' and before '('", {
  expect_warning(test_collection(
    "spacing", "spacing_if",
    transformer = style_text), NA)
})

test_that("space before comma is removed", {
  expect_warning(test_collection(
    "spacing", "spacing_comma",
    transformer = style_text), NA)
})


test_that("two commas are separated by a space", {
  expect_warning(test_collection(
    "spacing", "spacing_comma2",
    transformer = style_text), NA)
})

test_that("spacing between ! and bang is perserved", {
  expect_warning(test_collection(
    "spacing", "bang_bang_spacing",
    transformer = style_text), NA)
})

test_that("spacing around in works", {
  expect_warning(test_collection(
    "spacing", "spacing_in",
    transformer = style_text), NA)
})

test_that("no spaces after token FUNCTION", {
  expect_warning(test_collection(
    "spacing", "spacing_function",
    transformer = style_text, strict = FALSE), NA)
})

test_that("spacing around tilde", {
  expect_warning(test_collection(
    "spacing", "spacing-tilde",
    transformer = style_text, strict = TRUE), NA)
})



