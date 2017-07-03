context("indention operators")

test_that("pipe is indended correctly", {
  expect_warning(test_collection("indention_operators",
                                 "pipe_simple",
                                 transformer = style_op,
                                 write_back = TRUE), NA)
})

test_that("mathematical operators are indended correctly", {
  expect_warning(test_collection("indention_operators",
                                 "plus_minus",
                                 transformer = style_op,
                                 write_back = TRUE), NA)

  expect_warning(test_collection("indention_operators",
                                 "multiply_divide",
                                 transformer = style_op,
                                 write_back = TRUE), NA)
})


test_that("overall", {
  expect_warning(test_collection("indention_operators",
                                 "overall",
                                 transformer = style_text,
                                 write_back = TRUE), NA)
})
