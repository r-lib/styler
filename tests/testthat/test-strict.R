test_that("can style example source file with strict = TRUE", {
  expect_no_warning(test_collection(
    "strict", "strict",
    transformer = style_text,
    strict = TRUE
  ))
})

test_that("can style example source file with strict = FALSE", {
  expect_no_warning(test_collection(
    "strict", "non_strict",
    transformer = style_text,
    strict = FALSE
  ))
})

test_that("removes space at EOL", {
  expect_no_warning(test_collection(
    "strict", "eol",
    transformer = style_text,
    strict = FALSE
  ))
})

test_that("removes blank lines at EOF", {
  expect_no_warning(test_collection(
    "strict", "eof",
    transformer = style_text,
    strict = FALSE
  ))
})
