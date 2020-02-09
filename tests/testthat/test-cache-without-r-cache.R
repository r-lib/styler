test_that("Cache management fails mostly when R.cache is not installed", {
  skip_if(rlang::is_installed("R.cache"))
  expect_error(cache_info(), "is needed when the caching feature is activated")
  expect_error(activate_testthat_cache(), "is needed when the caching feature is activated")
  expect_error(cache_clear("testthat"), "is needed when the caching feature is activated")
  expect_error(capture.output(cache_deactivate()), NA)
  expect_error(
    assert_R.cache_installation(),
    "is needed when the caching feature is activated"
  )
})


test_that("styling works when R.cache is not installed", {
  skip_if(rlang::is_installed("R.cache"))
  # no warning when cache is disabled manually
  expect_output(
      withr::with_options(
        # simulate .onLoad() in fresh R session
        list(styler.cache_name = styler_version),
        {
          cache_deactivate()
          style_text("1+1")
        }
    ),
    "Deactivated cache."
  )

  # warning if cache is not disabled manually
  # warning for first time
  expect_warning(
    capture.output(
      withr::with_options(
        # simulate .onLoad() in fresh R session
        list(styler.cache_name = styler_version),
        style_text("1+1")
      )
    ),
    "R package R.cache .*Deactivating the caching feature for the current session"
  )

  # No warnings subsequently
  expect_warning(
    capture.output(
      withr::with_options(
        list(styler.cache_name = styler_version), {
          suppressWarnings(style_text("1+1"))
          style_text("1+1")
        }
      )
    ),
    NA
  )
})
