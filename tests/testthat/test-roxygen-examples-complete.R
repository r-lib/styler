context("test-roxygen-examples-complete")

test_that("analogous to test-roxygen-examples-complete", {

  expect_warning(test_collection(
    "roxygen-examples-complete",
    transformer = style_text
  ), NA)

})
