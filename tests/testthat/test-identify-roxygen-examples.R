context("test-roxygen-examples-identify.R")

#' Things to consider:
#' * one function declaration or many
#' * example(s) is last tag or not?
#' * malformatted examples
#' * \dontrun examples

test_that("one function, last tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/1-one-function-example-last-proper-run.R"
    )),
    list(c(6))
  )

  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/2-one-function-examples-last-proper-run.R"
    )),
    list(seq(6, 11))
  )
})

test_that("one function, not last, tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/3-one-function-example-not-last-proper-run.R"
    )),
    list(seq(5, 5))
  )

  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/4-one-function-examples-not-last-proper-run.R"
    )),
    list(seq(5, 9))
  )
})

test_that("multiple functions, last, tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/5-multiple-function-examples-last-proper-run.R"
    )),
    list(seq(5, 9), seq(17, 17))
  )
})

test_that("multiple functions, not last, tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_to_stop_of_roxygen_examples(testthat_file(
      "roxygen-examples-identify/6-multiple-function-examples-not-last-proper-run.R"
    )),
    list(seq(5, 5), seq(13, 17))
  )
})
