context("token-dependent indention")
test_that("token-dependent indention works in general", {
  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_mixed",
                                 transformer = style_text), NA)

  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_complex",
                                 transformer = style_text), NA)
})


test_that("token-dependent indention works with comments", {
  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_comments",
                                 transformer = style_text), NA)
})
