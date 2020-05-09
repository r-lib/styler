capture.output(test_that("Cache management works", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  # clearing a cache inactivates the caching functionality.
  expect_false(cache_info(format = "tabular")$activated)
  activate_testthat_cache()
  # at fresh startup
  expect_s3_class(cache_info(format = "tabular"), "tbl_df")
  expect_error(cache_info(), NA)
  expect_equal(basename(cache_activate()), styler_version)
  expect_equal(basename(cache_activate("xyz")), "xyz")
  expect_equal(getOption("styler.cache_name"), "xyz")
  # when cache xyz is activated, cache_info() shows deactivated for other caches
  expect_false(cache_info(styler_version, format = "tabular")$activated)
  expect_error(cache_info(format = "lucid"), NA)
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
}))

test_that("top-level test: Caches top-level expressions efficiently on style_text()", {
  on.exit(clear_testthat_cache())
  fresh_testthat_cache()
  text <- test_path("cache-with-r-cache/mlflow-1-in.R") %>%
    readLines()
  benchmark <- system.time(text_styled <- as.character(style_text(text)))
  expect_equal(text, text_styled)
  full_cached_benchmark <- system.time(text_styled2 <- as.character(style_text(text_styled)))
  expect_equal(text, text_styled2)

  # modify one function declaration
  text_styled[2] <- gsub(")", " )", text_styled[2], fixed = TRUE)
  partially_cached_benchmark <- system.time(
    text_cached_partially <- as.character(style_text(text_styled))
  )
  expect_equal(text, text_cached_partially)
  cache_deactivate()
  not_cached_benchmark <- system.time(
    text_not_cached <- as.character(style_text(text_styled))
  )
  expect_equal(text, text_not_cached)

  skip_on_cran()
  expect_lt(
    partially_cached_benchmark["elapsed"] * 2.5,
    not_cached_benchmark["elapsed"]
  )
  expect_lt(full_cached_benchmark["elapsed"] * 65, benchmark["elapsed"])
})


capture.output(test_that("cached expressions are displayed propperly", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_info <- cache_info("testthat", format = "tabular")
  expect_known_value(
    cache_info[, c("n", "size", "last_modified", "activated")],
    file = test_path("reference-objects/cache-info-1"),
    update = getOption("styler.test_dir_writable", TRUE)
  )

  activate_testthat_cache()
  style_text("1+1")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_known_value(
    cache_info[, c("n", "size", "activated")],
    file = test_path("reference-objects/cache-info-2"),
    update = getOption("styler.test_dir_writable", TRUE)

  )
  style_text("a <-function() NULL")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_known_value(
    cache_info[, c("n", "size", "activated")],
    file = test_path("reference-objects/cache-info-3"),
    update = getOption("styler.test_dir_writable", TRUE)

  )
}))

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})


test_that("When expressions are cached, number of newlines between them are preserved", {
  on.exit(clear_testthat_cache())
  fresh_testthat_cache()
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
