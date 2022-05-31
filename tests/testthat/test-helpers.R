

test_that("can construct and print vertical", {
  expect_snapshot({
    construct_vertical(c("1 + 1", "nw"))
  })
})


test_that("file types can be asserted", {
  expect_error(assert_filetype(".Rnw"), "case is ignored")
})

test_that("can lookup tokens", {
  expect_snapshot({
    lookup_new_special()
  })
})

test_that("can extend non-comment", {
  pd <- compute_parse_data_nested(c("if (TRUE) # \n call(34)"))
  expect_equal(extend_if_comment(pd$child[[1]], 4), 5)
})
