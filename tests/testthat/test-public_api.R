context("public API")

base <- rprojroot::find_testthat_root_file("public-api")

test_that("styler can style package", {
  expect_false(all(style_pkg(paste0(base, "/xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(paste0(base, "/xyzdir/"))))
})

test_that("styler can style file", {
  expect_false(
    style_file(paste0(base, "/xyzfile/random-script.R"), strict = FALSE)
  )
})

# style_active_file() must be tested manually.
