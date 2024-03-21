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

test_that("style_pkg() styles qmd files by default", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-qmd"))
  )
  expect_true(any(grepl("hello-world.R", msg, fixed = TRUE)))
  expect_true(any(grepl("test-package-xyz.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmd", msg, fixed = TRUE)))
  expect_true(any(grepl("random.Rmarkdown", msg, fixed = TRUE)))
  expect_true(any(grepl("README.Rmd", msg, fixed = TRUE)))
  expect_false(any(grepl("RcppExports.R", msg, fixed = TRUE)))
  expect_true(any(grepl("new.qmd", msg, fixed = TRUE)))
})

test_that("style_pkg() can find qmd anywhere", {
  msg <- capture_output(
    style_pkg(testthat_file("public-api", "xyzpackage-qmd"),
      filetype = ".Qmd"
    )
  )
  expect_no_match(msg, "hello-world.R", fixed = TRUE)
  expect_no_match(msg, "test-package-xyz.R", fixed = TRUE)
  expect_no_match(msg, "random.Rmd", fixed = TRUE)
  expect_no_match(msg, "random.Rmarkdown", fixed = TRUE)
  expect_no_match(msg, "README.Rmd", fixed = TRUE)
  expect_no_match(msg, "RcppExports.R", fixed = TRUE)
  expect_match(msg, "new.qmd", fixed = TRUE)
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
