test_that("roxyen code examples are written to cache as both individual expressions and as whole text", {
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
  # 1 code whole
  # 1 code by expr
  # 1 roxygen whole
  # 2 roxygen individual
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

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})

# consider dropping transformer text from cache key to speed up.
