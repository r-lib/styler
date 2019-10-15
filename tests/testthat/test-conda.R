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

  # with all files there
  expect_output(
    {
      git2r::init(path = tempdir)
      uninstall_precommit(scope = "repo", path_root = tempdir)
    },
    paste(
      "Uninstalled pre-commit from repo scope.*",
      "Removed \\.pre-commit-config\\.yaml.*"
    )
  )
  expect_false(fs::file_exists(fs::path(tempdir, ".pre-commit-config.yaml")))
  # second time
  expect_output(
    {
      git2r::init(path = tempdir)
      uninstall_precommit(scope = "repo", path_root = tempdir)
    },
    "You can re-install"
  )
  # when there is no pre-commit.yaml anymore
  use_precommit(tempdir, open = FALSE, force = TRUE)
  fs::file_delete(fs::path(tempdir, ".pre-commit-config.yaml"))
  expect_output(
    {
      git2r::init(path = tempdir)
      uninstall_precommit(scope = "repo", path_root = tempdir)
    },
    paste(
      "Uninstalled pre-commit from repo scope.*",
    )
  )
})
