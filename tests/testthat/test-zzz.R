test_that("styler tests did not use R.cache in user root", {
  skip_on_cran()
  skip_on_covr()
  expect_true(
    length(list.files(R.cache::getCachePath("styler"), recursive = TRUE)) == 0
  )
})

test_that("clear Cache", {
  R.cache::clearCache(R.cache::getCachePath("styler"), recursive = TRUE)
  skip_on_cran()
  skip_on_covr()
  expect_true(
    length(list.dirs(R.cache::getCachePath("styler"))) == 1
  )
})
