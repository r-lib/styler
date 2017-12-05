context("various helpers")

test_that("can construct vertical", {
  expect_error(construct_vertical(c("1 + 1", "nw")), NA)
})

test_that("can lookup tokens", {
  expect_error(lookup_new_special(), NA)
})

test_that("can extend non-comment", {
  pd <- compute_parse_data_nested(c("if (TRUE) # \n call(34)"))
  expect_equal(extend_if_comment(pd$child[[1]], 4), 5)
})
