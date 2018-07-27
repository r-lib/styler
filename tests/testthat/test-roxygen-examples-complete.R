context("test-roxygen-examples-complete")

test_that("analogous to test-roxygen-examples-complete", {
  expect_warning(test_collection(
    "roxygen-examples-complete", "^1[^1234567890]",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^11",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^12",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^13",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^14",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^2",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^3",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^4",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^5",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^6",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^7",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^8",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^9",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^10",
    transformer = style_text
  ), NA)
})
