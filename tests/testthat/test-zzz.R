test_that("tests don't write to styler-perm", {
  skip_on_cran()
  expect_false(fs::file_exists(
    fs::path(R.cache::getCacheRootPath(), "styler-perm")
  ))
})
