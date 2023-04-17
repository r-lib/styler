test_that("styler can style Rnw file", {
  expect_false({
    out <- style_file(
      testthat_file("public-api", "xyzfile-rnw", "random.Rnw"),
      strict = FALSE
    )
    out$changed
  })
  styled <- style_file(
    testthat_file("public-api", "xyzfile-rnw", "random2.Rnw"),
    strict = FALSE
  )
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
  local_test_setup()
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

  ## Rnw
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

test_that("Can display warning on unset styler cache", {
  withr::local_options(styler.cache_root = NULL)
  withr::local_seed(7)
  expect_message(
    ask_to_switch_to_non_default_cache_root(ask = TRUE),
    "See `?styler::caching`",
    fixed = TRUE
  )
})

test_that("No sensitive to decimal option", {
  withr::local_options(OutDec = ",")
  expect_snapshot({
    style_text("1")
  })
})

test_that("Can display warning on unset styler cache", {
  withr::local_options(styler.cache_root = "styler-perm")
  withr::local_seed(7)
  expect_silent(ask_to_switch_to_non_default_cache_root(ask = TRUE))
})


test_that("alignment detection can be turned off.", {
  withr::local_options(
    "styler.ignore_alignment" = TRUE,
    "styler.colored_print.vertical" = FALSE
  )
  text_in <- paste0(
    "call(\n",
    "  xb =  13,\n",
    "  t  = 'a'\n",
    ")"
  )
  text_out <- c(
    "call(",
    "  xb = 13,",
    "  t = \"a\"",
    ")"
  )

  expect_true(all(
    style_text(text_in) == text_out
  ))
})
