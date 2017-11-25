context("public API")



test_that("styler can style package", {
  expect_false(all(style_pkg(testthat_file("public-api", "xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(testthat_file("public-api", "xyzdir"))))
})

test_that("styler can style files", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile", "random-script.R"), strict = FALSE)
  )
  expect_false(any(style_file(
    rep(testthat_file("public-api", "xyzfile", "random-script.R"), 2),
    strict = FALSE
  )))
})


test_that("styler does not return error when there is no file to style", {
  expect_error(style_dir(testthat_file("public-api", "xyzemptydir"), strict = FALSE), NA)
})



##  ............................................................................
##  highlighted region                                                      ####

# styling active region cannot be tested automatically since
# rstudioapi::insertText() needs the context id.

context("public API - Rmd")

test_that("styler can style Rmd file", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random.Rmd"), strict = FALSE)
  )
  expect_warning(
    styled <- style_file(testthat_file("public-api", "xyzfile_rmd", "random2.Rmd"), strict = FALSE)
  )
  expect_false(styled)
})

test_that("styler handles malformed Rmd file and invalid R code in chunk", {
  expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random3.Rmd"), strict = FALSE)
  )
  expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random4.Rmd"), strict = FALSE)
  )
})

context("messages are correct")

test_that("messages of style_text are correct", {
  # Message if scope > line_breaks and code changes
  expect_message(style_text("1+1", scope = "tokens"))

  # No message if scope > line_breaks and code does not change
  expect_message(style_text("1 + 1", scope = "tokens"), NA)
  # No message if scope < spaces
  expect_message(style_text("1 + 1", scope = "spaces"), NA)

})

test_that("messages of style_file are correct", {
  # code changes, needs review
  temp_path <- copy_to_tempdir(testthat_file("public-api", "xyzdir-dirty", "dirty-sample.R"))
  expect_equal_to_reference(
    capture_messages(style_file(temp_path, scope = "tokens")),
    testthat_file("public-api/xyzdir-dirty/dirty-reference")
  )
  unlink(dirname(temp_path))
})
