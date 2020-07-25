test_that("base_indention is respected in caching", {
  on.exit(clear_testthat_cache())
  text <- c("1 + 1")
  fresh_testthat_cache()
  without_indention <- style_text(text)
  fresh_testthat_cache()
  style_text(text, base_indention = 5)
  expect_equal(
    style_text(text),
    without_indention
  )
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
