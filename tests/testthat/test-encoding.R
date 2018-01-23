context("non-ASCII characters are handled properly")

test_that("non-ASCII characters are handled properly", {
  # c.f. rlang::mut_latin1_locale()
  locale <- if (.Platform$OS.type == "windows") "English_United States.1252" else "en_US.ISO8859-1"

  withr::with_locale(
    c(LC_CTYPE = locale),
    {
      tmp <- tempfile(fileext = ".R")
      con <- file(tmp, encoding = "UTF-8")
      on.exit(close(con), add = TRUE)

      # c.f. dplyr's tests/testthat/helper-encoding.R
      writeLines("Gl\u00fcck+1", con)

      style_file(tmp)
      result <- readLines(con)
      expect_equal(result, "Gl\u00fcck + 1")
    }
  )
})
