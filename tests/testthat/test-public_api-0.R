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
      exclude_files = "\\.Rprofile"
    )
    nrow(styled) == 1
  }))

  capture_output(expect_true({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests",
      exclude_files = ".*ofile"
    )
    nrow(styled) == 1
  }))

  capture_output(expect_true({
    styled <- style_pkg(testthat_file("public-api", "xyzpackage"),
      exclude_dirs = "tests",
      exclude_files = "hello"
    )
    nrow(styled) == 0
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
