test_that("local_test_setup changes back to old cache location", {
  split_path <- function(x) {
    if (Sys.info()[1] == "Windows") {
      # not on other platforms as normalizePath messes up /private/var and /var
      x <- normalizePath(x, mustWork = FALSE)
    }
    unlist(strsplit(x, .Platform$file.sep, fixed = TRUE))
  }
  withr::defer(cache_deactivate(verbose = FALSE))
  old <- cache_info(format = "tabular")
  cache_activate(verbose = FALSE)

  test <- function() {
    local_test_setup()
    base <- split_path(tempfile())[2]
    expect_equal(
      split_path(cache_info(format = "tabular")$location)[2],
      base
    )
  }
  test()
  expect_equal(cache_info(format = "tabular")$location, old$location)
  expect_true(cache_info(format = "tabular")$activated)
})

test_that("local_test_setup changes back to old cache location", {
  old <- cache_info(format = "tabular")
  # don't activate
  split_path <- function(x) {
    if (Sys.info()[1] == "Windows") {
      # not on other platforms as normalizePath messes up /private/var and /var
      x <- normalizePath(x, mustWork = FALSE)
    }
    unlist(strsplit(x, .Platform$file.sep, fixed = TRUE))
  }

  test <- function() {
    local_test_setup()
    base <- split_path(tempfile())[2]
    expect_equal(
      split_path(cache_info(format = "tabular")$location)[2],
      base
    )
  }
  test()
  expect_equal(cache_info(format = "tabular")$location, old$location)
  expect_false(cache_info(format = "tabular")$activated)
})
