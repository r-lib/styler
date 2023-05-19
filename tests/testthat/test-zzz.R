test_that("styler tests did not use R.cache in user root", {
  skip_on_cran()
  skip_on_covr()
  expect_true(
    length(list.files(R.cache::getCachePath("styler"), recursive = TRUE)) == 0L
  )
})

test_that("clear Cache", {
  R.cache::clearCache(R.cache::getCachePath("styler"), recursive = TRUE)
  skip_on_cran()
  skip_on_covr()
  expect_true(
    length(list.dirs(R.cache::getCachePath("styler"))) == 1L
  )
})


test_that("can delete empty directory", {
  tmpdir <- withr::local_tempdir()
  withr::local_dir(tmpdir)
  dir.create("xxx")
  expect_true(delete_temp_directory_if_empty("xxx"))
  dir.create("xxx")
  file.create("xxx/yyy")
  list.files("xxx")
  expect_false(delete_temp_directory_if_empty("xxx"))
  expect_true(file.exists(tmpdir))
})
