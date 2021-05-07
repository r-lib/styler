test_that("snippet generation works", {
  local_test_setup(git = FALSE, use_precommit = FALSE, package = TRUE)
  usethis::use_package("styler")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    NA
  )
  expect_match(
    out, "^        - styler@.+$",
  )
  desc::desc_set("Remotes", "r-lib/styler")
  expect_warning(
    out <- capture_output(snippet_generate("additional-deps-roxygenize")),
    "you have remote dependencies "
  )
  expect_match(
    out, "^        - styler@.+$",
  )
})
