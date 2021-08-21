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
    "roxygen-examples-complete", "^12-fun",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^12-dont",
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
    "roxygen-examples-complete", "^2[^1234567890]",
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

  expect_warning(test_collection(
    "roxygen-examples-complete", "^15",
    transformer = style_text, scope = "spaces"
  ), NA)

  # Don't warn about empty strings in roxygen comments
  expect_warning(test_collection(
    "roxygen-examples-complete", "^16",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^17",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^18",
    transformer = style_text
  ), NA)
  expect_warning(test_collection(
    "roxygen-examples-complete", "^19",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^20",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^21",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^22",
    transformer = style_text
  ), NA)

  expect_error(test_collection(
    "roxygen-examples-complete", "^23",
    transformer = style_text
  ), "issues/1242")

  expect_warning(test_collection(
    "roxygen-examples-complete", "^24",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^25",
    transformer = style_text
  ), NA)
})
