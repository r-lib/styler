test_that("roxygen runs are done if necessary", {
  path <- fs::path(tempdir(), "a12")
  fs::dir_create(path)
  path_r <- fs::path(path, "R")
  fs::dir_create(path_r)
  setwd(path)
  withr::with_dir(
    path,
    {
      # when new lines are added
      text <- c("#' Roxygen comment", "#'", "#' more things", "NULL")
      writeLines(text, "R/first.R")
      writeLines(text, "R/second.R")
      expect_error(extract_diff_root("."), "is not a git repo")
      git2r::init(".")
      git2r::config(user.name = "testthat", user.email = "no-reply@testthat.com")
      expect_equal(extract_diff_root("."), NULL)
      expect_false(diff_requires_run_roxygenize("."))
      git2r::add(".", "R/first.R")
      git2r::status()

      expect_equal(extract_diff_root("."), text)
      expect_true(diff_requires_run_roxygenize("."))
      git2r::commit(".", "add roxgen2 file")

      # when non roxygen lines are added
      text2 <- c("x <- function()", "  NULL")
      writeLines(text2, "R/third.R")
      expect_equal(extract_diff_root("."), NULL)
      git2r::add(".", "R/third.R")
      expect_equal(extract_diff_root("."), add_trailing_linebreak(text2))
      expect_false(diff_requires_run_roxygenize("."))
      git2r::commit(".", "add non-roxygen2 file")

      # when roxygen line is replaced
      text[1] <- "# not roxygen, but replaced old "
      writeLines(text, "R/first.R")
      writeLines(text[1], "R/fourth.R")
      expect_equal(extract_diff_root("."), NULL)
      git2r::add(".", c("R/first.R", "R/fourth.R"))
      git2r::status()
      expect_equal(length(extract_diff_root(".")), 3)
      expect_true(diff_requires_run_roxygenize("."))
      git2r::commit(".", "replaced")


      # when roxygen line is removed
      writeLines("#", "R/first.R")

      expect_equal(extract_diff_root("."), NULL)
      git2r::add(".", "R/first.R")
      expect_equal(length(extract_diff_root(".")), 5)
      expect_true(diff_requires_run_roxygenize("."))
    }
  )
})
