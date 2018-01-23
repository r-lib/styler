context("non-ASCII characters are handled properly")

test_that("non-ASCII characters are handled properly", {
  # c.f. rlang::mut_latin1_locale()
  locale <- if (.Platform$OS.type == "windows") "English_United States.1252" else "en_US.ISO8859-1"

  withr::with_locale(
    c(LC_CTYPE = locale),
    {
      # c.f. dplyr's tests/testthat/helper-encoding.R
      latin_string <- "Gl\u00fcck+1"

      tmp <- tempfile(fileext = ".R")
      con <- file(tmp, encoding = "UTF-8")
      on.exit(close(con), add = TRUE)

      writeLines(latin_string, con)
      style_file(tmp)
      result <- readLines(con)
      expect_equal(result, latin_string)
    }
  )
})
