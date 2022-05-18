test_that("snippet generation works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  usethis::use_package("R", "Depends", "3.6.0")
  expect_error(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    NA,
  )
  expect_equal(out, "")
  usethis::use_package("styler")
  expect_error(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    NA,
  )

  expect_match(
    out, "    -   id: roxygenize\n.*        -    styler\n$",
  )
  desc::desc_set("Remotes", "r-lib/styler")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    "you have remote dependencies "
  )
  expect_match(
    out, "    -   id: roxygenize\n.*        -    styler\n$",
  )
})

test_that("snippet generation only includes hard dependencies", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE,
    install_hooks = FALSE, open = FALSE
  )
  usethis::use_package("styler")
  usethis::use_package("lintr", type = "Suggest")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    NA
  )
  expect_match(
    out, "    -   id: roxygenize\n.*        -    styler\n$",
  )
})


test_that("GitHub Action CI setup works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  use_precommit_config(
    root = getwd(),
    open = FALSE, verbose = FALSE
  )
  expect_error(use_ci("stuff"), "must be one of")
  use_ci("gha", root = getwd())
  expect_true(file_exists(".github/workflows/pre-commit.yaml"))
})

test_that("Pre-commit CI GitHub Action template is parsable", {
  expect_error(
    yaml::read_yaml(system.file("pre-commit-gha.yaml", package = "precommit")),
    NA
  )
})

test_that("Pre-commit CI setup works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  use_precommit_config(
    root = getwd(),
    open = FALSE, verbose = FALSE
  )
  use_ci(root = getwd(), open = FALSE)
  expect_false(file_exists(".github/workflows/pre-commit.yaml"))
})

test_that("Pre-commit CI setup works", {
  local_test_setup(
    git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE
  )
  expect_error(use_ci(root = getwd(), open = FALSE), "o `.pre-commit-config.yaml`")
})


test_that("Autoupdate is not conducted when renv present in incompatible setup", {
  skip_on_cran()

  # mock old pre-commit and renv versions
  mockery::stub(ensure_renv_precommit_compat, "version_precommit", "2.13.0")

  local_test_setup(
    git = TRUE, use_precommit = TRUE, install_hooks = FALSE, open = FALSE
  )
  initial <- rev_read() %>%
    rev_as_pkg_version()
  # simulate adding {renv}
  writeLines("", "renv.lock")

  # should downgrade rev
  expect_error(
    ensure_renv_precommit_compat(
      package_version_renv = package_version("0.13.0"), root = getwd()
    ),
    "Please update"
  )
  downgraded <- rev_read() %>%
    rev_as_pkg_version()
  expect_true(downgraded == initial)

  # simulate removing {renv} should be updated
  fs::file_delete("renv.lock")
  expect_warning(
    ensure_renv_precommit_compat(
      package_version("0.13.0"),
      root = getwd()
    ),
    NA
  )
})
