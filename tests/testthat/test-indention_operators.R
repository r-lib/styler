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


test_that("overall", {
  expect_warning(test_collection("indention_operators",
                                 "overall",
                                 transformer = style_text), NA)
})
