if (!on_cran()) {
  test_that("can install pre-commit", {
    skip_if(not_conda())
    expect_error(install_precommit(), NA)
    expect_error(install_precommit(force = TRUE), NA)
  })

  test_that("conda 3.10 should now work", {
    expect_true(Sys.Date() < as.Date("2022-03-30"))
  })

  test_that("can use pre-commit", {
    tempdir <- local_test_setup(quiet = FALSE, install_hooks = FALSE)
    expect_message(
      use_precommit(open = FALSE, force = TRUE, install_hooks = FALSE, root = tempdir),
      "to get the latest"
    )
    expect_message(
      use_precommit(open = FALSE, force = FALSE, install_hooks = FALSE, root = tempdir),
      "There is already "
    )
  })

  test_that("fails early if repo is not a git repo ", {
    tempdir <- local_test_setup(git = FALSE, quiet = FALSE, install_hooks = FALSE)

    expect_error(
      use_precommit(open = FALSE, install_hooks = FALSE, root = tempdir),
      "is not a git repo"
    )
  })

  test_that("can use custom config file ", {
    tempdir1 <- local_test_setup(install_hooks = FALSE)
    tempdir2 <- local_test_setup(install_hooks = FALSE)

    path_custom <- fs::path(tempdir2, "some-precommit.yaml")
    new_text <- "# 4js93"
    readLines(system.file("pre-commit-config-proj.yaml", package = "precommit")) %>%
      c(new_text) %>%
      writeLines(path_custom)
    git2r::init(tempdir1)
    use_precommit(config_source = path_custom, open = FALSE, force = TRUE, install_hooks = FALSE, root = tempdir1)
    config <- readLines(fs::path(tempdir1, ".pre-commit-config.yaml"))
    expect_equal(
      config[length(config)],
      new_text
    )
  })

  test_that("existing hooks are recognized", {
    tempdir <- local_test_setup(quiet = FALSE, install_hooks = FALSE)
    withr::with_dir(tempdir, {
      git2r::init()
      usethis::proj_set(".")
      usethis::use_readme_rmd(open = FALSE)
      withr::local_options(precommit.block_install_hooks = TRUE)

      # usethis hook is removed without error
      expect_message(
        use_precommit(legacy_hooks = "forbid", open = FALSE, install_hooks = FALSE, root = "."),
        "Removed the render-README hook,"
      )
      writeLines(letters, ".git/hooks/pre-commit")
      expect_error(
        use_precommit(legacy_hooks = "forbid", open = FALSE, install_hooks = FALSE, root = "."),
        "existing hooks installed"
      )

      # tolerate other hook scripts in migration mode
      expect_message(
        use_precommit(legacy_hooks = "allow", force = TRUE, open = FALSE, install_hooks = FALSE, root = "."),
        "Running in migration"
      )

      # can also remove other hooks
      writeLines(letters, ".git/hooks/pre-commit")
      expect_message(
        use_precommit(legacy_hooks = "remove", force = TRUE, open = FALSE, install_hooks = FALSE, root = "."),
        "Sucessfully installed"
      )
    })
  })


  test_that("Can uninstall pre-commit (repo scope)", {
    # with all files there
    tempdir <- local_test_setup(use_precommit = TRUE, quiet = FALSE, install_hooks = FALSE)
    expect_message(
      uninstall_precommit(scope = "repo", root = tempdir),
      "Uninstalled pre-commit from repo scope.*"
    )
    expect_false(file_exists(fs::path(tempdir, ".pre-commit-config.yaml")))
    expect_false(any(grepl(".pre-commit", readline(fs::path(tempdir, ".Rbuildignore")))))
    # second time
    expect_message(
      uninstall_precommit(scope = "repo", root = tempdir),
      "You can re-install"
    )

    # when there is no pre-commit.yaml anymore
    suppressMessages(use_precommit(open = FALSE, force = TRUE, install_hooks = FALSE, root = tempdir))
    fs::file_delete(fs::path(tempdir, ".pre-commit-config.yaml"))
    expect_message(
      uninstall_precommit(scope = "repo", root = tempdir),
      paste("Uninstalled pre-commit from repo scope.*")
    )
  })

  test_that("Can uninstall (userly)", {
    if (not_conda()) {
      tempdir <- local_test_setup(use_precommit = TRUE, quiet = FALSE, install_hooks = FALSE)
      expect_error(
        uninstall_precommit(scope = "user", ask = "none", root = tempdir),
        "installed with conda"
      )
    } else {
      tempdir <- local_test_setup(use_precommit = FALSE, quiet = FALSE, install_hooks = FALSE)
      expect_message(
        uninstall_precommit(scope = "user", ask = "none", root = tempdir),
        "Removed pre-commit from"
      )
      expect_error(
        uninstall_precommit(scope = "user", ask = "none", root = tempdir),
        "No installation found."
      )
    }
  })

  test_that("use_precommit fails when no user installation is found", {
    skip_if(not_conda())
    expect_error(use_precommit(open = FALSE, install_hooks = FALSE, root = tempdir), "installed on your system")
  })

  test_that("can install pre-commit with remote config", {
    if (!not_conda()) {
      expect_error(install_precommit(), NA)
    }
    tempdir <- local_test_setup(quiet = FALSE, install_hooks = FALSE)
    expect_message(
      use_precommit(
        example_remote_config(),
        open = FALSE, force = TRUE, install_hooks = FALSE, root = tempdir
      ),
      "to get the latest"
    )
  })

  test_that("fails gracefully when there are", {
    if (!not_conda()) {
      expect_message(install_precommit(), "already installed")
    }
    tempdir <- local_test_setup(use_precommit = FALSE, quiet = FALSE, install_hooks = FALSE)
    withr::local_options(precommit.block_install_hooks = TRUE)
    withr::with_dir(
      tempdir,
      {
        withr::defer(call_and_capture("git", "config --unset-all core.hooksPath"))
        call_and_capture("git", "config core.hooksPath .githooks")
        expect_error(
          use_precommit(open = FALSE, force = TRUE, install_hooks = FALSE, root = tempdir),
          "stdout: [ERROR] Cowardly refusing to install hooks with `core.hooksPath` set.",
          fixed = TRUE
        )
      }
    )
  })

  test_that("fails gracefully when reticulate is not available", {
    if (not_conda()) {
      expect_error(install_precommit(), "Please install the R package reticulate")
    }
  })
  test_that("can update via conda", {
    if (not_conda()) {
      expect_error(
        with_mock(update_precommit(), "precommit:::assert_reticulate_is_installed" = function(...) NULL),
        paste(
          "You can only update your pre-commit executable via the R API if you",
          "chose the installation method via conda"
        )
      )

      expect_match(version_precommit(), "[0-9]+\\.[0-9]+\\.[0-9]+")
    } else {
      uninstall_precommit(scope = "user", ask = "none", root = ".")
      version <- "2.10.0"
      reticulate::conda_install("r-precommit", paste0("pre-commit==", version))
      expect_equal(version_precommit(), version)
      expect_invisible(update_precommit(), 0)
      expect_false(version_precommit() == version)
    }
  })
}
