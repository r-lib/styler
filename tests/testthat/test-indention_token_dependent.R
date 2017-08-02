context("token-dependent indention")
test_that("token-dependent indention works", {
  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_mixed",
                                 transformer = style_text), NA)
})


test_that("token-dependent indention works", {
  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_comments",
                                 transformer = style_text), NA)
})


test_that("forced regex token-dependent indention", {
  expect_warning(test_collection("indention_token_dependent",
                                 "token_dependent_force",
                                 transformer = style_text), NA)
})
