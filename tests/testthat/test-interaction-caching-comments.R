test_that("Correclty removes comments that are not top-level when making pd shallow (low-level)", {
  local_test_setup(cache = TRUE)
  text7 <- c(
    "call(",
    "  # inline-comment",
    "  1 + 1,",
    "  f(),",
    "  x(5)",
    ")",
    "# styler"
  )
  style_text(text7) # only making pd shallow when call is cached.
  pd_flat <- text_to_flat_pd(text7, tidyverse_style(), more_specs = cache_more_specs_default())
  expect_false(any(pd_flat$text == "# inline-comment"))
})

test_that("Correclty removes comments that are not top-level when making pd shallow (high-level)", {
  local_test_setup(cache = TRUE)
  text7 <- c(
    "call(",
    "# styler: off",
    "1 +1,",
    "f(),",
    "    x(5))",
    "# styler"
  )
  style_text(text7)
  text7[length(text7)] <- "# comment"
  expect_equal(
    style_text(text7) %>% as.character(),
    text7
  )
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
