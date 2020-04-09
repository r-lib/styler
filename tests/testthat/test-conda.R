tempdir <- fs::path(tempdir(), "test-precommit")
fs::dir_create(tempdir)
git2r::init(path = tempdir)

test_that("can install pre-commit", {
  skip_if(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))
  expect_error(install_precommit(), NA)
})

test_that("can use pre-commit", {
  expect_output(
    use_precommit(open = FALSE, force = TRUE, path_root = tempdir),
    "to get the latest"
  )
  expect_output(
    use_precommit(open = FALSE, force = FALSE, path_root = tempdir),
    "There is already "
  )
})

test_that("fails early if repo is not a git repo ", {
  expect_error(
    {
      tempdir <- fs::path(tempdir(), "t9")
      fs::dir_create(tempdir)
      use_precommit(path_root = tempdir)
    },
    "is not a git repo"
  )
})

test_that("can use custom config file ", {
  tempdir <- fs::path(tempdir(), "t10")
  fs::dir_create(tempdir)
  tempdir2 <- fs::path(tempdir(), "t11")
  fs::dir_create(tempdir2)
  path_custom <- fs::path(tempdir2, "some-precommit.yaml")
  new_text <- "# 4js93"
  readLines(system.file("pre-commit-config-proj.yaml", package = "precommit")) %>%
    c(new_text) %>%
    writeLines(path_custom)
  git2r::init(tempdir)
  use_precommit(path_cp_config_from = path_custom, path_root = tempdir, force = TRUE)
  config <- readLines(fs::path(tempdir, ".pre-commit-config.yaml"))
  expect_equal(
    config[length(config)],
    new_text
  )
})



test_that("Can uninstall pre-commit (repo scope)", {
  # with all files there
  expect_output(
    uninstall_precommit(scope = "repo", path_root = tempdir),
    paste(
      "Uninstalled pre-commit from repo scope.*",
      "Removed \\.pre-commit-config\\.yaml.*"
    )
  )
  expect_false(fs::file_exists(fs::path(tempdir, ".pre-commit-config.yaml")))
  # second time
  expect_output(
    uninstall_precommit(scope = "repo", path_root = tempdir),
    "You can re-install"
  )

  # when there is no pre-commit.yaml anymore
  use_precommit(open = FALSE, force = TRUE, path_root = tempdir)
  fs::file_delete(fs::path(tempdir, ".pre-commit-config.yaml"))
  expect_output(
    uninstall_precommit(scope = "repo", path_root = tempdir),
    paste("Uninstalled pre-commit from repo scope.*")
  )
})

test_that("Can uninstall (globally)", {
  if (isTRUE(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))) {
    expect_error(
      uninstall_precommit(scope = "global", ask = "none", path_root = tempdir),
      "installed with conda"
    )
  } else {
    expect_output(
      uninstall_precommit(scope = "global", ask = "none"),
      "Removed pre-commit from"
    )
    expect_error(
      uninstall_precommit(scope = "global", ask = "none"),
      "No installation found."
    )
  }
})

test_that("use_precommit fails when no global installation is found", {
  skip_if(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))
  expect_error(use_precommit(path_root = tempdir), "installed on your system")
})

test_that("can install pre-commit with remote config", {
  if (!isTRUE(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))) {
    expect_error(install_precommit(), NA)
  }

  expect_output(
    {
      git2r::init(path = tempdir)
      use_precommit(example_remote_config(),
        open = FALSE, force = TRUE, path_root = tempdir
      )
    },
    "to get the latest"
  )
})

test_that("fails gracefully when there are", {
  if (!isTRUE(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))) {
    expect_error(install_precommit(), NA)
  }
  withr::with_dir(
    tempdir,
    {
      git2r::init()
      on.exit(call_and_capture("git", "config --unset-all core.hooksPath"))
      call_and_capture("git", "config core.hooksPath .githooks")
      expect_error(
        use_precommit(open = FALSE, force = TRUE, path_root = tempdir),
        "stdout: [ERROR] Cowardly refusing to install hooks with `core.hooksPath` set.",
        fixed = TRUE
      )
    }
  )
})


test_that("fails gracefully when reticulate is not available", {
  if (isTRUE(as.logical(Sys.getenv("EXTERNAL_INSTALLATION")))) {
    expect_error(install_precommit(), "Please install the R package reticulate")
  }
})
