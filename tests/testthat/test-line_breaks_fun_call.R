context("line breaks for function calls")
test_that("line breaks work in general", {
  expect_warning(test_collection("line_breaks_fun_call",
                                 "token_dependent_mixed",
                                 transformer = style_text), NA)

  expect_warning(test_collection("line_breaks_fun_call",
                                 "token_dependent_complex",
                                 transformer = style_text), NA)
})


test_that("line breaks work with comments", {
  expect_warning(test_collection("line_breaks_fun_call",
                                 "token_dependent_comments",
                                 transformer = style_text), NA)
})

test_that("line breaks work with exceptions", {
  expect_warning(test_collection("line_breaks_fun_call",
                                 "switch_ifelse",
                                 transformer = style_text), NA)
})
