test_that("Cache management fails mostly when R.cache is not installed", {
  skip_if(rlang::is_installed("R.cache"))
  expect_error(cache_info(), "is needed when the caching feature is activated")
  expect_error(cache_activate(), "is needed when the caching feature is activated")
  expect_error(cache_clear(), "is needed when the caching feature is activated")
  expect_error(capture.output(cache_deactivate()), NA)
  expect_silent(assert_R.cache_installation())
  expect_error(
    assert_R.cache_installation(installation_only = TRUE),
    "is needed when the caching feature is activated"
  )
})


test_that("styling works when R.cache is not installed", {
  skip_if(rlang::is_installed("R.cache"))
  # warning for first time
  expect_warning(
    capture.output(
      withr::with_options(
        # simulate .onLoad() in fresh R session
        list(styler.cache_name = cache_derive_name()),
        style_text("1+1")
      )
    ),
    "Deactivating the caching feature for the current session"
  )

  # No warnings subsequently
  expect_warning(
    capture.output(
      withr::with_options(
        list(styler.cache_name = cache_derive_name()), {
          suppressWarnings(style_text("1+1"))
          style_text("1+1")
        }
      )
    ),
    NA
  )
})
