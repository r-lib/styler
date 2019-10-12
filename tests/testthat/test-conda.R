test_that("can install pre-commit", {
  expect_success(install_precommit())
  expect_success(use_precommit(open = FALSE, force = TRUE))
})
