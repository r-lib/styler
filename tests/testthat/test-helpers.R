test_that("can construct and print vertical", {
  skip_if_not_installed("prettycode")
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
