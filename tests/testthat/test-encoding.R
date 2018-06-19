context("non-ASCII characters are handled properly")

test_that("non-ASCII characters are handled properly for text styling", {
  expect_equal(
    style_text("glück <-3") %>% unclass(), "glück <- 3"
  )
})


test_that("non-ASCII characters are handled properly for file styling", {
  skip_if(.Platform$OS.type != "windows")

  withr::with_locale(
    c(LC_CTYPE = "English_United States.1252"),
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
