context("public API")



test_that("styler can style package", {
  capture_output(expect_false({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"))
    any(styled$changed)
  }))
})

test_that("styler can style directory", {
  capture_output(expect_false({
    styled <- style_dir(testthat_file("public-api", "xyzdir"))
    any(styled$changed)
  }))
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
    testthat_file("public-api", "xyzemptydir"),
    strict = FALSE
  ), NA))
})

context("public API - Rmd in style_file()")

test_that("styler can style Rmd file", {
  capture_output(expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_rmd", "random.Rmd"),
      strict = FALSE
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
  if (cli::is_utf8_output()) {
    # if utf8 is available test under this and test  if it is not available
    encodings <- list(TRUE)
  } else {
    # if utf8 is not available, only test under that
    encodings <- list()
  }
  for (encoding in c(encodings, FALSE)) {
    withr::with_options(
      list(cli.unicode = encoding), {
        # Message if scope > line_breaks and code changes
        temp_path <- copy_to_tempdir(testthat_file(
          "public-api", "xyzdir-dirty", "dirty-sample-with-scope-tokens.R"
        ))
        expect_equal_to_reference(
          capture.output(
            style_file(temp_path, scope = "tokens")
          ),
          testthat_file(paste0(
            "public-api/xyzdir-dirty/dirty-reference-with-scope-tokens-",
            ifelse(cli::is_utf8_output(), "utf8", "non-utf8")
          ))
        )
        unlink(dirname(temp_path))

        # No message if scope > line_breaks and code does not change
        temp_path <- copy_to_tempdir(testthat_file(
          "public-api", "xyzdir-dirty", "clean-sample-with-scope-tokens.R"
        ))
        expect_equal_to_reference(
          capture.output(style_file(temp_path, scope = "tokens")),
          testthat_file(paste0(
            "public-api/xyzdir-dirty/clean-reference-with-scope-tokens-",
            ifelse(cli::is_utf8_output(), "utf8", "non-utf8")
          ))
        )
        unlink(dirname(temp_path))

        # No message if scope <= line_breaks even if code is changed.
        temp_path <- copy_to_tempdir(testthat_file(
          "public-api", "xyzdir-dirty", "dirty-sample-with-scope-spaces.R"
        ))
        expect_equal_to_reference(
          capture.output(style_file(temp_path, scope = "spaces")),
          testthat_file(paste0(
            "public-api/xyzdir-dirty/dirty-reference-with-scope-spaces-",
            ifelse(cli::is_utf8_output(), "utf8", "non-utf8")
          ))
        )
        unlink(dirname(temp_path))
      }
    )
  }
})

context("public API - Rmd in style_dir()")

test_that("styler can style R and Rmd files via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = c("R", "Rmd")
    )
  )
  expect_true(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
})

test_that("styler can style Rmd files only via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = "Rmd"
    )
  )
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
})

test_that("styler can style .r and .rmd files via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = c(".r", ".rmd")
    )
  )
  expect_true(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
})

context("public API - Rmd in style_pkg()")

test_that("styler can style R and Rmd files via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rmd"),
      filetype = c("R", "Rmd")
    )
  )
  expect_true(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_true(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("README.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
})

test_that("styler can style Rmd files only via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rmd"),
      filetype = "Rmd"
    )
  )
  expect_false(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_false(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("README.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
})

test_that("insufficient R version returns error", {
  expect_error(stop_insufficient_r_version())
})

context("public API - Rnw in style_file()")

test_that("styler can style Rnw file", {
  capture_output(expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile-rnw", "random.Rnw"),
      strict = FALSE
    )
    out$changed
  }))

  capture_output(expect_warning(
    styled <- style_file(testthat_file("public-api", "xyzfile-rnw", "random2.Rnw"), strict = FALSE)
  ))
  expect_false(styled$changed)
})

test_that("styler handles malformed Rnw file and invalid R code in chunk", {
  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile-rnw", "random3.Rnw"), strict = FALSE)
  ))

  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile-rnw", "random4.Rnw"), strict = FALSE)
  ))
})

context("public API - Rnw in style_pkg()")

test_that("styler can style R, Rmd and Rnw files via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rnw"),
      filetype = c("R", "Rmd", "Rnw")
    )
  )
  expect_true(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_true(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rnw", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
})

test_that("styler can style Rnw files only via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rnw"),
      filetype = "Rnw"
    )
  )
  expect_false(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_false(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_false(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rnw", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
})
