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

test_that("include_roxygen_exmples is respected in caching", {
  on.exit(clear_testthat_cache())
  text <- c("#' Roxygen", "#'", "#' @examples", "#' 1+1", "1 + 1")
  fresh_testthat_cache()
  with_examples <- style_text(text)
  fresh_testthat_cache()
  style_text(text, include_roxygen_examples = FALSE)
  expect_equal(
    style_text(text, include_roxygen_examples = TRUE),
    with_examples
  )
})


test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
