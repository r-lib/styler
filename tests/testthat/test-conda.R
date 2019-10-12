test_that("can install pre-commit", {
  expect_error(install_precommit(), NA)
  tempdir <- fs::path(tempdir(), "test-precommit")
  fs::dir_create(tempdir)
  on.exit(fs::dir_delete(tempdir))
  expect_output(
    {
      git2r::init(path = tempdir)
      use_precommit(tempdir, open = FALSE, force = TRUE)
    },
    "to get the latest"
  )
})
