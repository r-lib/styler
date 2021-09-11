test_that("base_indention is respected in caching", {
  local_test_setup(cache = TRUE)
  text <- c("1 + 1")
  without_indention <- style_text(text)
  local_test_setup(cache = TRUE)
  style_text(text, base_indention = 5)
  expect_equal(
    style_text(text),
    without_indention
  )
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
