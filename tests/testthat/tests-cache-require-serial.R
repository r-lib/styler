test_that("top-level test: Caches top-level expressions efficiently on style_text()", {
  local_test_setup(cache = TRUE)
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
  skip_on_covr()
  expect_lt(
    partially_cached_benchmark["elapsed"] * 2.4,
    not_cached_benchmark["elapsed"]
  )
  expect_lt(full_cached_benchmark["elapsed"] * 45, benchmark["elapsed"])
})


test_that("roxygen code examples are written to cache as whole expressions bring speedgain", {
  skip_on_cran()
  local_test_setup(cache = TRUE)
  text <- readLines(test_path("cache-with-r-cache/roxygen-cache-1.R"))
  first <- system.time(styled <- style_text(text))
  # don't use full cache, only roxygen cache
  styled[1] <- "#' This is a nother text"
  second <- system.time(style_text(styled))
  expect_gt(first["elapsed"], 4 * second["elapsed"])
})
