test_that("local_test_setup changes back to old cache location", {
  withr::defer(cache_deactivate(verbose = FALSE))
  old <- cache_info(format = "tabular")
  cache_activate(verbose = FALSE)

  test <- function() {
    local_test_setup()
    base <- unlist(strsplit(tempfile(), .Platform$file.sep, fixed = TRUE))[2]
    expect_equal(
      unlist(strsplit(cache_info(format = "tabular")$location, .Platform$file.sep, fixed = TRUE))[2],
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

  test <- function() {
    local_test_setup()
    base <- unlist(strsplit(tempfile(), .Platform$file.sep, fixed = TRUE))[2]
    expect_equal(
      unlist(strsplit(cache_info(format = "tabular")$location, .Platform$file.sep, fixed = TRUE))[2],
      base
    )
  }
  test()
  expect_equal(cache_info(format = "tabular")$location, old$location)
  expect_false(cache_info(format = "tabular")$activated)
})
