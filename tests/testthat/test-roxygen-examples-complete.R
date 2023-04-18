test_that("analogous to test-roxygen-examples-complete", {
  expect_warning(test_collection(
    "roxygen-examples-complete", "^01",
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
    "roxygen-examples-complete", "^02",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^03",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^04",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^05",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^06",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^07",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^08",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^09",
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

  expect_warning(test_collection(
    "roxygen-examples-complete", "^26",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^27",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^28",
    transformer = style_text
  ), NA)

  expect_warning(test_collection(
    "roxygen-examples-complete", "^29",
    transformer = style_text
  ), NA)
})
