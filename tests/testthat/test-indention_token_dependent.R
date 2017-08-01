context("token-dependent indention")
test_that("token-dependent indention works", {
  expect_warning(test_collection("indention_token_dependent",
                                 transformer = style_text), NA)
})
