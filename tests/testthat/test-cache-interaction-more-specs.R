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

test_that("ignore_alignment is respected in caching", {
  local_test_setup(cache = TRUE)
  text <- c("call(", "  arxone = 1,", "  tw3    = 2", ")")
  text_without_alignment <- c("call(", "  arxone = 1,", "  tw3 = 2", ")")
  with_detection <- style_text(text)
  withr::local_options(styler.ignore_alignment = TRUE)
  without_detection <- style_text(text)
  expect_equal(
    as.character(without_detection),
    as.character(text_without_alignment)
  )
  expect_equal(cache_info(format = "tabular")$n, 2)
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
