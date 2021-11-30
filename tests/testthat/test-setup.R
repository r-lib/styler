test_that("snippet generation works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  usethis::use_package("styler")
  usethis::use_package("R", "Depends", "3.6.0")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    NA
  )
  expect_match(
    out, "    -   id: roxygenize\n.*        -    styler\n        -    testthat\n$",
  )
  desc::desc_set("Remotes", "r-lib/styler")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    "you have remote dependencies "
  )
  expect_match(
    out, "    -   id: roxygenize\n.*        -    styler\n        -    testthat\n$",
  )
})


test_that("GitHub Action CI setup works", {
  expect_error(use_ci("stuff"), "must be one of")
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  use_ci("gha", root = getwd())
  expect_true(file_exists(".github/workflows/pre-commit.yaml"))
})

test_that("Pre-commit CI setup works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  use_ci(root = getwd())
  expect_false(file_exists(".github/workflows/pre-commit.yaml"))
})
