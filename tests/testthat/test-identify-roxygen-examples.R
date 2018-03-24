context("test-identify-roxygen-examples.R")

#' Things to consider:
#' * one function declaration or many
#' * example(s) is last tag or not?
#' * malformatted examples
#' * \dontrun examples

test_that("one function, last tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_stop_ofroyxgen_examples_from_paths(testthat_file(
    "identify-roxygen-examples/1-one-function-example-last-proper-run.R"
    )),
    list(c(6, 6))
  )

  expect_equal(
    identify_start_stop_ofroyxgen_examples_from_paths(testthat_file(
      "identify-roxygen-examples/2-one-function-examples-last-proper-run.R"
    )),
    list(c(6, 11))
  )
})

test_that("one function, not last, tag, properly formatted, no dontrun", {
  expect_equal(
    identify_start_stop_ofroyxgen_examples_from_paths(testthat_file(
      "identify-roxygen-examples/3-one-function-example-not-last-proper-run.R"
    )),
    list(c(5, 9))
  )

  expect_equal(
    identify_start_stop_ofroyxgen_examples_from_paths(testthat_file(
      "identify-roxygen-examples/4-one-function-examples-not-last-proper-run.R"
    )),
    list(c(5, 5))
  )
})
