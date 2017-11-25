context("public API")



test_that("styler can style package", {
  capture_output(expect_false(all(style_pkg(testthat_file("public-api", "xyzpackage")))))
})

test_that("styler can style directory", {
  capture_output(expect_false(all(style_dir(testthat_file("public-api", "xyzdir")))))
})

test_that("styler can style files", {
  capture_output(expect_false({
    out <- style_file(testthat_file("public-api", "xyzfile", "random-script.R"), strict = FALSE)
    out$changed
  }))

  capture_output(expect_false(any({
    out <- style_file(
      rep(testthat_file("public-api", "xyzfile", "random-script.R"), 2),
      strict = FALSE
    )
    out$changed
  })))
})


test_that("styler does not return error when there is no file to style", {
  capture_output(expect_error(style_dir(
    testthat_file("public-api", "xyzemptydir"), strict = FALSE), NA
  ))
})



##  ............................................................................
##  highlighted region                                                      ####

# styling active region cannot be tested automatically since
# rstudioapi::insertText() needs the context id.

context("public API - Rmd")

test_that("styler can style Rmd file", {
  capture_output(expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_rmd", "random.Rmd"), strict = FALSE
    )
    out$changed
  }))

  capture_output(expect_warning(
    styled <- style_file(testthat_file("public-api", "xyzfile_rmd", "random2.Rmd"), strict = FALSE)
  ))
  expect_false(styled$changed)
})

test_that("styler handles malformed Rmd file and invalid R code in chunk", {
  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random3.Rmd"), strict = FALSE)
  ))

  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random4.Rmd"), strict = FALSE)
  ))
})

context("messages are correct")

test_that("messages (via cat()) of style_file are correct", {
  # Message if scope > line_breaks and code changes
  temp_path <- copy_to_tempdir(testthat_file("public-api", "xyzdir-dirty", "dirty-sample-with-scope-tokens.R"))
  expect_equal_to_reference(capture_output(
    enc::to_utf8(style_file(temp_path, scope = "tokens"))),
    testthat_file("public-api/xyzdir-dirty/dirty-reference-with-scope-tokens")
  )
  unlink(dirname(temp_path))

  # No message if scope > line_breaks and code does not change
  temp_path <- copy_to_tempdir(testthat_file("public-api", "xyzdir-dirty", "clean-sample-with-scope-tokens.R"))
  expect_equal_to_reference(
    enc::to_utf8(capture_output(style_file(temp_path, scope = "tokens"))),
    testthat_file("public-api/xyzdir-dirty/clean-reference-with-scope-tokens")
  )
  unlink(dirname(temp_path))

  # No message if scope <= line_breaks even if code is changed.
  temp_path <- copy_to_tempdir(testthat_file("public-api", "xyzdir-dirty", "dirty-sample-with-scope-spaces.R"))
  expect_equal_to_reference(
    enc::to_utf8(capture_output(style_file(temp_path, scope = "spaces"))),
    testthat_file("public-api/xyzdir-dirty/dirty-reference-with-scope-spaces")
  )
  unlink(dirname(temp_path))
})

