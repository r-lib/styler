context("test-utils")

test_that("non-comment-helpers", {
  pd <- compute_parse_data_nested("a <- # hi \n x %>% b()")
  child <- pd$child[[1]]
  expect_equal(previous_non_comment(child, 4), 2)
  expect_equal(next_non_comment(child, 2), 4)
})
