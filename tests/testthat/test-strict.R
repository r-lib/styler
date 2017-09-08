context("test strict")

test_that("can style example source file with strict = TRUE", {
  expect_warning(test_collection(
    "strict", "strict",
    transformer = style_text,
    strict = TRUE), NA)
})

test_that("can style example source file with strict = FALSE", {
  expect_warning(test_collection(
    "strict", "non_strict",
    transformer = style_text,
    strict = FALSE), NA)
})

test_that("removes space at EOL", {
  expect_warning(test_collection(
    "strict", "eol",
    transformer = style_text,
    strict = FALSE), NA)
})

test_that("removes blank lines at EOF", {
  expect_warning(test_collection(
    "strict", "eof",
    transformer = style_text,
    strict = FALSE), NA)
})


test_that("Space placed after 'if' and before '('", {
  expect_equal(style_text(c("if(TRUE) x else y")), "if (TRUE) x else y")
})

test_that("space before comma is removed", {
  expect_equal(style_text("c(    1,       16    , 333 , 33 ,  1)"),
                          "c(1, 16, 333, 33, 1)")
})
