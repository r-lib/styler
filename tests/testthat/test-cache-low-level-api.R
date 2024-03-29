test_that("caching utils make right blocks with semi-colon", {
  blocks_simple_uncached <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    base::transform(is_cached = FALSE) %>%
    cache_find_block()
  expect_equal(blocks_simple_uncached, c(1, 1, 1, 1))

  blocks_simple_cached <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    base::transform(is_cached = TRUE) %>%
    cache_find_block()
  expect_equal(blocks_simple_cached, c(1, 1, 1, 1))

  blocks_edge <- compute_parse_data_nested(c("1 + 1", "2; 1+1")) %>%
    base::transform(is_cached = c(TRUE, TRUE, FALSE, FALSE)) %>%
    cache_find_block()
  expect_equal(blocks_edge, c(1, 2, 2, 2))
})

test_that("caching utils make right blocks with comments", {
  text <- '
   ### comment
   x = 1 ### comment
   y = 2 # comment
   x<-1 ###comment
   y <- 2 # comment
   "a string here"

   # something something
   tau1 = 1 # here?
   '


  blocks_simple_uncached <- compute_parse_data_nested(text) %>%
    base::transform(is_cached = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
      TRUE, FALSE, FALSE, FALSE
    )) %>%
    cache_find_block()
  expect_equal(blocks_simple_uncached, c(1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4))

  text <- "
   ### comment
   x = 1
   y = 2 # comment
   x<-1
   y <- 2 # comment

   # something something
   tau1 = 1 # here?
   "
  blocks_simple_cached <- compute_parse_data_nested(text) %>%
    base::transform(is_cached = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
    )) %>%
    cache_find_block()
  expect_equal(blocks_simple_cached, c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
})


test_that("blank lines are correctly identified", {
  local_test_setup(cache = TRUE)

  text <- c(
    "1 + 1",
    "",
    "",
    "f(x)",
    "",
    "",
    "",
    "x < 3",
    "function() NULL"
  )
  # when not cached, all code in same block
  more_specs <- cache_more_specs_default()
  pd_nested <- compute_parse_data_nested(text,
    transformers = tidyverse_style(),
    more_specs = more_specs
  )
  cache_by_expression(text, tidyverse_style(), more_specs = more_specs)
  expect_equal(
    pd_nested$block, rep(1, 4)
  )

  expect_equal(
    find_blank_lines_to_next_block(pd_nested),
    1
  )

  # when partly cached, not all code in same block
  text[4] <- "f (x)"
  pd_nested <- compute_parse_data_nested(text, tidyverse_style(), more_specs = more_specs)
  expect_equal(
    pd_nested$block, c(1, 2, 3, 3)
  )

  expect_equal(
    find_blank_lines_to_next_block(pd_nested),
    c(1, 3, 4)
  )
})

test_that("caching utils make right blocks with comments", {
  blocks_simple_uncached <- compute_parse_data_nested(c("1 + 1", "2 # comment")) %>%
    base::transform(is_cached = FALSE) %>%
    cache_find_block()
  expect_equal(blocks_simple_uncached, c(1, 1, 1))

  blocks_simple_cached <- compute_parse_data_nested(c("1 + 1", "2 # comment2")) %>%
    base::transform(is_cached = TRUE) %>%
    cache_find_block()
  expect_equal(blocks_simple_cached, c(1, 1, 1))

  blocks_edge <- compute_parse_data_nested(c("1 + 1", "2 # 1+1")) %>%
    base::transform(is_cached = c(TRUE, TRUE, FALSE)) %>%
    cache_find_block()
  expect_equal(blocks_edge, c(1, 2, 2))
})


################################################################################

test_that("Individual comment expressions are not cached", {
  local_test_setup(cache = TRUE)
  style_text(c("# g", "1"))
  cache_info <- cache_info(format = "tabular")
  # because output text is cached as a whole, there should be 2 cached
  # expressions now
  expect_equal(cache_info$n, 2)
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
