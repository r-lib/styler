test_that("roxzgen code examples are written to cache as both individual expressions and as whole text", {
  local_test_setup(cache = TRUE)
  more_specs <- cache_more_specs_default()
  text <- c(
    "#' Comment",
    "#'",
    "#' Stuff",
    "#' @examples",
    "#' 1 + 1",
    "#' f(x )",
    "NULL",
    "103"
  )
  styled <- style_text(text)
  expect_equal(cache_info(format = "tabular")$n, 6)
  # 1 whole (with comments)
  # 1code whole
  # 1 code by expr
  # 1 roxzgen whole
  # 2 roxzgen individula
  # total: 6
  expect_true(
    is_cached(as.character(styled), tidyverse_style(), more_specs = more_specs)
  )
  expect_true(
    is_cached(c("", "1 + 1", "f(x)"), tidyverse_style(), more_specs = more_specs)
  )
  expect_true(
    is_cached(c("1 + 1"), tidyverse_style(), more_specs = more_specs)
  )
  expect_true(
    is_cached(c("f(x)"), tidyverse_style(), more_specs = more_specs)
  )
  expect_true(
    is_cached(c("NULL"), tidyverse_style(), more_specs = more_specs)
  )
  expect_true(
    is_cached(c("103"), tidyverse_style(), more_specs = more_specs)
  )
  expect_false(
    is_cached(c("f(x )"), tidyverse_style(), more_specs = more_specs)
  )
})


test_that("roxzgen code examples are written to cache as whole expressions bring speedgain", {
  skip_on_cran()
  local_test_setup(cache = TRUE)
  text <- readLines(test_path("cache-with-r-cache/roxygen-cache-1.R"))
  first <- system.time(styled <- style_text(text))
  # don't use full cache, only roxygen cache
  styled[1] <- "#' This is a nother text"
  second <- system.time(style_text(styled))
  expect_gt(first["elapsed"], 5 * second["elapsed"])
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})

# consider dropping transformer text from cache key to speed up.
