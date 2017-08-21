context("indention operators")

test_that("pipe is indended correctly", {
  expect_warning(test_collection("indention_operators",
                                 "pipe",
                                 transformer = style_text,
                                 write_back = TRUE), NA)
})

test_that("mathematical operators are indended correctly", {
  expect_warning(test_collection("indention_operators",
                                 "plus_minus",
                                 transformer = style_op), NA)

  expect_warning(test_collection("indention_operators",
                                 "multiply_divide",
                                 transformer = style_op), NA)
})


test_that("while / for / if without curly brackets", {
  expect_warning(test_collection("indention_operators",
                                 "while_for_if_without_curly",
                                 transformer = style_text), NA)
})

test_that("logical, special and EQ_SUB tokens are indented correctly", {
  expect_warning(test_collection("indention_operators",
                                 "logical_special",
                                 transformer = style_text), NA)

  expect_warning(test_collection("indention_operators",
                                 "eq",
                                 transformer = style_text), NA)
})

test_that("dollar is indented and spaced correctly", {
  expect_warning(test_collection("indention_operators",
                                 "dollar",
                                 transformer = style_text), NA)
})

test_that(
  "code is indented correctly if not first pontial trigger causes indention", {
    expect_warning(
      test_collection(
        "indention_operators", "not_first_trigger",
        transformer = style_text
      ),
    NA)
})


test_that("overall", {
  expect_warning(test_collection("indention_operators",
                                 "overall",
                                 transformer = style_text), NA)
})
