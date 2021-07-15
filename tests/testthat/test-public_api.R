context("public API")



test_that("styler can style package", {
  capture_output(expect_false({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"))
    any(styled$changed)
  }))
})

test_that("styler can style package and exclude some directories", {
  capture_output(
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests"
    )
  )
  expect_true(nrow(styled) == 1)
  expect_false(any(grepl("tests/testthat/test-package-xyz.R", styled$file)))
})

test_that("styler can style package and exclude some sub-directories", {
  capture_output(
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests/testthat"
    )
  )
  expect_true(nrow(styled) == 2)
  expect_true(any(grepl("tests/testthat.R", styled$file)))
  expect_false(any(grepl("tests/testthat/test-package-xyz.R", styled$file)))
})



test_that("styler can style package and exclude some directories and files", {
  capture_output(expect_true({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests",
      exclude_files = ".Rprofile"
    )
    nrow(styled) == 1
  }))

  capture_output(expect_true({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests",
      exclude_files = "./.Rprofile"
    )
    nrow(styled) == 1
  }))
})


test_that("styler can style directory", {
  capture_output(expect_false({
    styled <- style_dir(testthat_file("public-api", "xyzdir"))
    any(styled$changed)
  }))
})

test_that("styler can style directories and exclude", {
  capture_output(expect_true({
    styled <- style_dir(
      testthat_file("public-api", "renvpkg"),
      exclude_dirs = "renv"
    )
    nrow(styled) == 2
  }))
  capture_output(expect_true({
    styled <- style_dir(
      testthat_file("public-api", "renvpkg"),
      exclude_dirs = c("renv", "tests/testthat")
    )
    nrow(styled) == 1
  }))

  capture_output(expect_true({
    styled <- style_dir(
      testthat_file("public-api", "renvpkg"),
      exclude_dirs = "./renv"
    )
    nrow(styled) == 2
  }))

  capture_output(expect_true({
    styled <- style_dir(
      testthat_file("public-api", "renvpkg"),
      exclude_dirs = "./renv", recursive = FALSE
    )
    nrow(styled) == 0
  }))

  capture_output(expect_true({
    styled <- style_dir(
      testthat_file("public-api", "renvpkg"),
      recursive = FALSE
    )
    nrow(styled) == 0
  }))
})

test_that("styler can style files", {
  # just one
  capture_output(expect_equivalent(
    {
      out <- style_file(c(
        testthat_file("public-api", "xyzfile", "random-script.R")
      ), strict = FALSE)
      out$changed
    },
    rep(FALSE, 1)
  ))
  # multiple not in the same working directory
  capture_output(expect_equivalent(
    {
      out <- style_file(c(
        testthat_file("public-api", "xyzfile", "random-script.R"),
        testthat_file("public-api", "xyzfile", "subfolder", "random-script.R")
      ), strict = FALSE)
      out$changed
    },
    rep(FALSE, 2)
  ))
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

test_that("styler can style Rmarkdown file", {
  capture_output(expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile_rmd", "random.Rmarkdown"),
      strict = FALSE
    )
    out$changed
  }))

  capture_output(expect_warning(
    styled <- style_file(testthat_file("public-api", "xyzfile_rmd", "random2.Rmarkdown"), strict = FALSE)
  ))
  expect_false(styled$changed)
})

test_that("styler handles malformed Rmd file and invalid R code in chunk", {
  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random4.Rmd"), strict = FALSE),
    "3: "
  ))

  capture_output(expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random7.Rmd"), strict = FALSE),
    "Malformed file"
  ))
})

context("messages are correct")

test_that("messages (via cat()) of style_file are correct", {
  for (encoding in ls_testable_encodings()) {
    withr::with_options(
      list(cli.unicode = encoding == "utf8"),
      {
        # Message if scope > line_breaks and code changes
        output <- catch_style_file_output(file.path(
          "public-api",
          "xyzdir-dirty",
          "dirty-sample-with-scope-tokens.R"
        ))
        expect_known_value(
          output,
          testthat_file(paste0(
            "public-api/xyzdir-dirty/dirty-reference-with-scope-tokens-",
            encoding
          )),
          update = getOption("styler.test_dir_writable", TRUE)
        )

        # No message if scope > line_breaks and code does not change
        output <- catch_style_file_output(file.path(
          "public-api", "xyzdir-dirty", "clean-sample-with-scope-tokens.R"
        ))
        expect_known_value(
          output,
          testthat_file(paste0(
            "public-api/xyzdir-dirty/clean-reference-with-scope-tokens-",
            encoding
          )),
          update = getOption("styler.test_dir_writable", TRUE)
        )

        # No message if scope <= line_breaks even if code is changed.
        output <- catch_style_file_output(file.path(
          "public-api", "xyzdir-dirty", "dirty-sample-with-scope-spaces.R"
        ))
        expect_known_value(
          output,
          testthat_file(paste0(
            "public-api/xyzdir-dirty/dirty-reference-with-scope-spaces-",
            encoding
          )),
          update = getOption("styler.test_dir_writable", TRUE)
        )
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

context("public API - Rmd in style_dir()")

test_that("styler can style R, Rmd and Rmarkdown files via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = c("R", "Rmd", "Rmarkdown")
    )
  )
  expect_true(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmarkdown", msg, fixed = TRUE)))
})

test_that("styler can style Rmd files only via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = "Rmd"
    )
  )
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_false(any(grepl("random-rmd-script.Rmarkdown", msg, fixed = TRUE)))
})

test_that("styler can style .r and .rmd files only via style_dir()", {
  msg <- capture_output(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
      filetype = c(".r", ".rmd")
    )
  )
  expect_true(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("random-rmd-script.Rmarkdown", msg, fixed = TRUE)))
})

context("public API - Rmd in style_pkg()")

test_that("styler can style R and Rmd files via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rmd"),
      filetype = c("R", "Rmd", "Rmarkdown")
    )
  )
  expect_true(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_true(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmarkdown", msg, fixed = TRUE)))
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
  expect_false(any(grepl("random.Rmarkdown", msg, fixed = TRUE)))
  expect_true(any(grepl("README.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
})

test_that("styler can style Rmarkdown files only via style_pkg()", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-rmd"),
              filetype = "Rmarkdown"
    )
  )
  expect_false(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_false(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_false(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmarkdown", msg, fixed = TRUE)))
  expect_false(any(grepl("README.Rmd", msg, fixed = TRUE)))
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

test_that("dry run options work:", {
  path <- test_path("public-api/dry/unstyled.R")
  # test the testing function
  expect_error(test_dry(path, style_file, styled = TRUE))

  # real tests
  ## R
  test_dry(path, style_file)
  path <- test_path("public-api/dry/styled.R")
  test_dry(path, style_file, styled = TRUE)

  ## Rmd
  test_dry(test_path("public-api/dry/unstyled.Rmd"), style_file, styled = FALSE)
  test_dry(test_path("public-api/dry/styled.Rmd"), style_file, styled = TRUE)

  ## Rmd
  test_dry(test_path("public-api/dry/unstyled.Rnw"), style_file, styled = FALSE)
  test_dry(test_path("public-api/dry/styled.Rnw"), style_file, styled = TRUE)
})

test_that("base indention works", {
  # for single-line strings
  n_spaces <- 5
  text_in <- "x<- function() NULL"
  expect_equal(
    style_text(text_in, base_indention = n_spaces),
    construct_vertical(paste0(add_spaces(n_spaces), style_text(text_in)))
  )
  # for multi-line strings
  text_in <- c(
    "x<- function()",
    '"here\nis"',
    "NULL",
    "1+ 1"
  )
  text_out <- c(
    "     x <- function() {",
    '       "here',
    'is"',
    "     }",
    "     NULL",
    "     1 + 1"
  )
  expect_equal(
    as.character(style_text(text_in, base_indention = n_spaces)),
    text_out
  )
})

test_that("scope can be specified as is", {
  capture_output(expect_false({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"), scope = I("spaces"))
    any(styled$changed)
  }))

  file <- testthat_file("public-api", "xyzpackage", "R", "hello-world.R")
  capture_output(expect_false({
    styled <- style_file(file, scope = I("line_breaks"))
    any(styled$changed)
  }))
  expect_equal(
    style_text(c("1+14;x=2"), scope = I(c("line_breaks", "tokens"))),
    construct_vertical(c("1+14", "x<-2"))
  )
})

test_that("Can properly determine style_after_saving", {
  withr::with_envvar(list(save_after_styling = TRUE), {
    expect_warning(op <- save_after_styling_is_active(), "is depreciated")
    expect_equal(op, TRUE)
  })

  withr::with_envvar(list(save_after_styling = FALSE), {
    expect_warning(op <- save_after_styling_is_active(), "is depreciated")
    expect_equal(op, FALSE)
  })


  withr::with_options(list(styler.save_after_styling = TRUE), {
    expect_silent(op <- save_after_styling_is_active())
    expect_equal(op, TRUE)
  })

  withr::with_options(list(styler.save_after_styling = TRUE), {
    withr::with_envvar(list(save_after_styling = FALSE), {
      expect_warning(op <- save_after_styling_is_active(), "is depreciated")
      expect_equal(op, TRUE)
    })
  })

  withr::with_options(list(styler.save_after_styling = FALSE), {
    expect_silent(op <- save_after_styling_is_active())
    expect_equal(op, FALSE)
  })
})
