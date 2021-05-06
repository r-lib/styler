test_that("snippet generation works", {
  withr::local_options("usethis.quiet" = TRUE)
  withr::local_dir(withr::local_tempdir())
  pkg_name <- "testPkg2"
  usethis::create_package(pkg_name)
  withr::local_dir(pkg_name)
  usethis:::proj_set(".")
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
