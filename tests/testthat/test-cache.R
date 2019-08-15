test_that("activated cache brings speedup", {
  withr::with_options(
    list("styler.use_cache" = TRUE, "styler.cache_subdir" = "testthat"), {
      cache_clear(ask = FALSE)
      first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      expect_true(first["elapsed"] / 2 > second["elapsed"])
    }
  )
})

test_that("unactivated cache does not bring speedup", {
  withr::with_options(
    list("styler.use_cache" = FALSE, "styler.cache_subdir" = "testthat"), {
      cache_clear(ask = FALSE)
      first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
      expect_false(first["elapsed"] / 2 > second["elapsed"])
    }
  )
})
