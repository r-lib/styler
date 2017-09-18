context("public API")



test_that("styler can style package", {
  expect_false(all(style_pkg(testthat_file("public-api", "xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(testthat_file("public-api", "xyzdir"))))
})

test_that("styler can style file", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile", "random-script.R"), strict = FALSE)
  )
})

test_that("styler does not return error when there is no file to style", {
  expect_error(style_dir(testthat_file("public-api", "xyzemptydir"), strict = FALSE), NA)
})



##  ............................................................................
##  highlighted region                                                      ####

# styling active region cannot be tested automatically since
# rstudioapi::insertText() needs the context id.
