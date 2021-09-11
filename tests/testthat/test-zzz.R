test_that("styler tests did not use R.cache in user root", {
  R.cache::clearCache(R.cache::getCachePath("styler"))
  skip_on_cran()
  expect_true(
    length(list.dirs(R.cache::getCachePath("styler"))) == 1
  )
})
