context("circumvent parsing bugs")

test_that("repreated parsing solves wrong parent assignment", {
  expect_warning(test_collection(
    "parsing", "repeated_parsing",
    transformer = style_text,
    strict = FALSE),
    NA)

  # move to temp dir
  dir <- tempfile("styler")
  dir.create(dir)
  path_temp <- file.path(dir, "repeated_parsing-in.R")
  path_perm <- testthat_file("parsing", "repeated_parsing-in.R")
  file.copy(path_perm, dir)

  sys_call <- paste0(
    "R -q -e \"styler::style_file(\\\"", path_temp, "\\\")\""
  )
  calls_sys(sys_call, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  ref <- xfun::read_utf8(testthat_file("parsing", "repeated_parsing-out.R"))
  result <- xfun::read_utf8(path_temp)
  expect_equal(ref, result)
  unlink(dir)
})

test_that("long strings are parsed correctly", {
  if (getRversion() < "3.2") skip("skip on R < 3.2 because of parsing problems")

  expect_warning(
    test_collection("parsing", "long_strings", transformer = style_text),
    NA
  )
})

test_that("issues with parsing long strings on R 3.1 can be detected", {
  if (getRversion() >= "3.2") {
    skip("skip on R >= 3.2 because parsing probmes don't appear")
  }
  expect_error(
    test_collection("parsing", "long_strings", transformer = style_text),
    "install R .* 3.2"
  )
})


test_that("CRLF EOLs fail with informative error", {
  skip_if(getRversion() < "3.4")

  expect_error(
    style_text("glück <- 3\r\n glück + 1"),
    "Please change the EOL character in your editor to Unix style and try again."
  )
  expect_error(
    style_text(c("glück <- 3", "glück + 1\r\n 3")),
    "Please change the EOL character in your editor to Unix style and try again."
  )
})


test_that("mixed CRLF / LF EOLs fail", {
  expect_error(
    style_text("a + 3 -4 -> x\nx + 2\r\n glück + 1"),
    "unexpected input"
  )
})
