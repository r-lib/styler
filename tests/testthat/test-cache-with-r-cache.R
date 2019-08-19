test_that("No warnings are issued when R.cache is installed", {
  skip_if_not_installed("R.cache")
  on.exit(cache_deactivate())
  expect_silent(assert_R.cache_installation(installation_only = TRUE))
  expect_silent(assert_R.cache_installation())
  expect_warning(style_text("1+1"), NA)
  cache_activate()
  assert_R.cache_installation()
  expect_warning(style_text("1+1"), NA)
})


test_that("Cache management works when R.cache is installed", {
  skip_if_not_installed("R.cache")
  on.exit(cache_deactivate())
  cache_activate()
  # at fresh startup, with R.cache installed
  expect_s3_class(cache_info(format = "tabular"), "tbl_df")
  expect_error(cache_info(), NA)
  expect_equal(basename(cache_activate()), utils::packageDescription("styler", fields = "Version"))
  expect_equal(basename(cache_activate("xyz")), "xyz")
  expect_equal(getOption("styler.cache_name"), "xyz")
  cache_deactivate()
  expect_false(cache_info(format = "tabular")$activated)
  expect_equal(getOption("styler.cache_location"), NULL)
  expect_error(cache_clear(ask = FALSE), NA)
})



test_that("activated cache brings speedup", {
  skip_if_not_installed("R.cache")
  cache_clear(ask = FALSE)
  cache_activate()
  on.exit(cache_deactivate())
  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
})


test_that("unactivated cache does not bring speedup", {
  skip_if_not_installed("R.cache")
      cache_clear(ask = FALSE)
      cache_deactivate()
      first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      expect_false(first["elapsed"] / 2 > second["elapsed"])
})
