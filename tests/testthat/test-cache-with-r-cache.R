test_that("Cache management works", {
  # clearing a cache inactivates the caching functionality.
  expect_false(cache_info(format = "tabular")$activated)
  local_test_setup(cache = TRUE)
  # at fresh startup
  expect_s3_class(cache_info(format = "tabular"), "data.frame")
  expect_error(capture.output(cache_info()), NA)
  expect_equal(basename(cache_activate()), styler_version)
  expect_equal(basename(cache_activate("xyz")), "xyz")
  expect_equal(getOption("styler.cache_name"), "xyz")
  # when cache xyz is activated, cache_info() shows deactivated for other caches
  expect_false(cache_info(styler_version, format = "tabular")$activated)
  expect_error(capture.output(cache_info(format = "lucid")), NA)
  # cache_info() defaults to the currently active cache
  expect_equal(basename(cache_info(format = "tabular")$location), "xyz")

  cache_deactivate()
  # cache_info() defaults to the cache of the version of styler if
  # not cache is active
  expect_equal(
    basename(cache_info(format = "tabular")$location), styler_version
  )
  expect_false(cache_info(format = "tabular")$activated)
  expect_equal(getOption("styler.cache_location"), NULL)
  expect_error(cache_clear("testthat", ask = FALSE), NA)
})

test_that("cached expressions are displayed propperly", {
  skip_if(getRversion() < "4.2")

  cache_info <- cache_info("testthat", format = "tabular")
  expect_snapshot({
    cache_info[, c("n", "size", "last_modified", "activated")]
  })

  local_test_setup(cache = TRUE)
  style_text("1+1")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_snapshot({
    cache_info[, c("n", "size", "activated")]
  })
  style_text("a <-function() NULL")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_snapshot({
    cache_info[, c("n", "size", "activated")]
  })
})


test_that("When expressions are cached, number of newlines between them are preserved", {
  local_test_setup(cache = TRUE)
  text <- c(
    "1 + 1",
    "",
    "",
    "f(x)",
    "",
    "",
    "",
    "x < 3",
    "function() NULL"
  )
  # add to cache
  expect_equal(text[1:4], as.character(style_text(text[1:4])))
  # applied cache
  expect_equal(text[1:4], as.character(style_text(text[1:4])))

  expect_equal(text, as.character(style_text(text)))
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
