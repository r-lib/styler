test_that("snippet generation works", {
  local_test_setup(git = FALSE, use_precommit = FALSE, package = TRUE, install_hooks = FALSE)
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
