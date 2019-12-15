styler_version <- utils::packageDescription("styler", fields = "Version")
clear_testthat_cache <- purrr::partial(cache_clear, "testthat", ask = FALSE)

capture.output(test_that("No warnings are issued when R.cache is installed", {
  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache())
  expect_silent(assert_R.cache_installation(installation_only = TRUE))
  expect_silent(assert_R.cache_installation())
  expect_warning(style_text("1+1"), NA)
  expect_warning(cache_activate("testthat"), NA)
  expect_warning(style_text("1+1"), NA)
  expect_silent(assert_R.cache_installation(installation_only = TRUE))
  expect_silent(assert_R.cache_installation())
}))

capture.output(test_that("Cache management works when R.cache is installed", {
  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  # clearing a cache inactivates the caching functionality.
  expect_false(cache_info(format = "tabular")$activated)
  cache_activate("testthat")
  # at fresh startup, with R.cache installed
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
  expect_error(cache_clear(ask = FALSE), NA)
}))



capture.output(test_that("activated cache brings speedup on style_file() API", {
  skip_if_not_installed("R.cache")
  cache_activate("testthat")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")
  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))

text <- c(
  "#' Roxygen",
  "#' Comment",
  "#' @examples",
  "#' 1 + 1",
  "k <- function() {",
  "  1 + 1",
  "  if (x) {",
  "    k()",
  "  }",
  "}",
  ""
) %>%
  rep(10)

capture.output(test_that("activated cache brings speedup on style_text() API on character vector", {
  skip_if_not_installed("R.cache")
  cache_activate("testthat")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")

  first <- system.time(styler::style_text(text))
  second <- system.time(styler::style_text(text))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))

capture.output(test_that("activated cache brings speedup on style_text() API on character scalar", {
  skip_if_not_installed("R.cache")
  cache_activate("testthat")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")

  first <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(test_that("no speedup when tranformer changes", {
  skip_if_not_installed("R.cache")
  cache_activate("testthat")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")
  t1 <- tidyverse_style()
  first <- system.time(style_text(text, transformers = t1))
  t1$use_raw_indention <- !t1$use_raw_indention
  second <- system.time(style_text(text, transformers = t1))
  expect_false(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(
  test_that(paste0(
    "activated cache brings speedup on style_text() API on ",
    "character scalar and character vector (mixed)"
  ), {
  skip_if_not_installed("R.cache")
  cache_activate("testthat")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")

  first <- system.time(styler::style_text(text))
  second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(test_that("unactivated cache does not bring speedup", {
  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache)
  clear_testthat_cache()
  cache_deactivate()
  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_false(first["elapsed"] / 2 > second["elapsed"])
}))

capture.output(test_that("cached expressions are displayed propperly", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_info <- cache_info("testthat", format = "tabular")
  expect_known_value(
    cache_info[, c("n", "size", "last_modified", "activated")],
    file = test_path("reference-objects/cache-info-1")
  )

  cache_activate("testthat")
  style_text("1+1")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_known_value(
    cache_info[, c("n", "size", "activated")],
    file = test_path("reference-objects/cache-info-2")
  )
  style_text("a <-function() NULL")
  cache_info <- cache_info(format = "tabular")
  cache_info$size <- round(cache_info$size, -2)
  expect_known_value(
    cache_info[, c("n", "size", "activated")],
    file = test_path("reference-objects/cache-info-3")
  )
}))
