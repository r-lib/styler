test_that("caching utils make right blocks with semi-colon", {
  blocks_simple_uncached <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    dplyr::mutate(is_cached = FALSE) %>%
    cache_find_block()
  expect_equal(blocks_simple_uncached, c(1, 1, 1, 1))

  blocks_simple_cached <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    dplyr::mutate(is_cached = TRUE) %>%
    cache_find_block()
  expect_equal(blocks_simple_cached, c(1, 1, 1, 1))

  blocks_edge <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    dplyr::mutate(is_cached = c(TRUE, TRUE, FALSE, FALSE)) %>%
    cache_find_block()
  expect_equal(blocks_edge, c(1, 2, 2, 2))
})


test_that("caching utils make right blocks with comments", {
  blocks_simple_uncached <- compute_parse_data_nested(c("1 + 1", "2 # comment")) %>%
    dplyr::mutate(is_cached = FALSE) %>%
    cache_find_block()
  expect_equal(blocks_simple_uncached, c(1, 1, 1))

  blocks_simple_cached <- compute_parse_data_nested(c("1 + 1", "2 # comment2")) %>%
    dplyr::mutate(is_cached = TRUE) %>%
    cache_find_block()
  expect_equal(blocks_simple_cached, c(1, 1, 1))

  blocks_edge <- compute_parse_data_nested(c("1 + 1", "2 # 1+1")) %>%
    dplyr::mutate(is_cached = c(TRUE, TRUE, FALSE)) %>%
    cache_find_block()
  expect_equal(blocks_edge, c(1, 2, 2))
})


################################################################################

test_that("Individual comment expressions are not cached", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_activate("testthat")
  style_text(c("# g", "1"))
  cache_info <- cache_info()
  # because output text is cached as a whole, there should be 2 cached
  # expressions now
  expect_equal(cache_info$n, 2)
})
