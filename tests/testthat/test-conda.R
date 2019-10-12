test_that("can install pre-commit", {
  expect_success(install_precommit())
  tempdir <- fs::path(tempdir(), "test-precommit")
  on.exit(fs::dir_delete(tempdir))
  expect_success(withr::with_dir(
    fs::dir_create(tempdir), {
      git2r::init()
      use_precommit(open = FALSE, force = TRUE)
    }
  ))
})
