test_that("styler tests did not use R.cache in user root", {
  skip_on_cran()
  skip_on_covr()
  skip_during_parallel()
  expect_true(
    length(list.files(R.cache::getCachePath("styler"), recursive = TRUE)) == 0L
  )
})

test_that("clear Cache", {
  # if R CMD CHECK is running, R.cache root is set to a temp
  # directory by default.
  # https://github.com/HenrikBengtsson/R.cache/commit/c7ac171f15f035674346d5504049c38cf07c268f
  # Hence, this clean up won't clean up the user directory that Ripley is
  # concerned about, but only some temp directory.
  # On the other hand, it seems completely unclear how the user cache even gets
  # populated.
  # skip_during_parallel()
  cache_path <- R.cache::getCachePath("styler")
  R.cache::clearCache(cache_path, recursive = TRUE, prompt = FALSE)
  # rlang::abort(paste0('cache path:', cache_path))
  skip_on_cran()
  skip_on_covr()
  expect_true(
    length(list.dirs(R.cache::getCachePath("styler"))) == 1L
  )
})
