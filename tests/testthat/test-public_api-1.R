test_that("styler can style files", {
  # just one
  capture_output(expect_equal(
    {
      out <- style_file(c(
        testthat_file("public-api", "xyzfile", "random-script.R")
      ), strict = FALSE)
      out$changed
    },
    rep(FALSE, 1),
    ignore_attr = TRUE
  ))
  # multiple not in the same working directory
  capture_output(expect_equal(
    {
      out <- style_file(c(
        testthat_file("public-api", "xyzfile", "random-script.R"),
        testthat_file("public-api", "xyzfile", "subfolder", "random-script.R")
      ), strict = FALSE)
      out$changed
    },
    rep(FALSE, 2),
    ignore_attr = TRUE
  ))
})


test_that("styler does not return error when there is no file to style", {
  capture_output(expect_error(style_dir(
    testthat_file("public-api", "xyzemptydir"),
    strict = FALSE
  ), NA))
})



test_that("styler can style Rmd file", {
  expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_rmd", "random.Rmd"),
      strict = FALSE
    )
    out$changed
  })

  styled <- style_file(
    testthat_file("public-api", "xyzfile_rmd", "random2.Rmd"),
    strict = FALSE
  )
  expect_false(styled$changed)
})

test_that("styler can style Rmarkdown file", {
  expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_rmd", "random.Rmarkdown"),
      strict = FALSE
    )
    out$changed
  })


  styled <- style_file(
    testthat_file("public-api", "xyzfile_rmd", "random2.Rmarkdown"),
    strict = FALSE
  )
  expect_false(styled$changed)
})


test_that("styler can style qmd file", {
  expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_qmd", "new.qmd"),
      strict = FALSE
    )
    out$changed
  })

  styled <- style_file(
    testthat_file("public-api", "xyzfile_rmd", "random2.Rmarkdown"),
    strict = FALSE
  )
  expect_false(styled$changed)
})

test_that("styler handles malformed Rmd file and invalid R code in chunk", {
  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "invalid4.Rmd"), strict = FALSE),
    "3: "
  ))

  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "invalid7.Rmd"), strict = FALSE),
    "Malformed file"
  ))
})




test_that("messages (via cat()) of style_file are correct", {
  for (encoding in ls_testable_encodings()) {
    withr::with_options(
      list(cli.unicode = encoding == "utf8"),
      {
        # Message if scope > line_breaks and code changes
        expect_snapshot({
          cat(catch_style_file_output(file.path(
            "public-api",
            "xyzdir-dirty",
            "dirty-sample-with-scope-tokens.R"
          )), sep = "\n")
        })

        # No message if scope > line_breaks and code does not change
        expect_snapshot({
          cat(catch_style_file_output(file.path(
            "public-api", "xyzdir-dirty", "clean-sample-with-scope-tokens.R"
          )), sep = "\n")
        })

        # No message if scope <= line_breaks even if code is changed.
        expect_snapshot({
          cat(catch_style_file_output(file.path(
            "public-api", "xyzdir-dirty", "dirty-sample-with-scope-spaces.R"
          )), sep = "\n")
        })
      }
    )
  }
})

test_that("Messages can be suppressed", {
  withr::with_options(
    list(styler.quiet = TRUE),
    {
      output <- catch_style_file_output(file.path(
        "public-api", "xyzdir-dirty", "dirty-sample-with-scope-spaces.R"
      ))
      expect_equal(output, character(0))
    }
  )
})
