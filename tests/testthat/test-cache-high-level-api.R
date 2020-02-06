capture.output(test_that("activated cache brings speedup on style_file() API", {
  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))

text <- c(
  "#' Roxygen",
  "#' Comment",
  "#' @examples",
  "#' 1 + 1",
  "k <- function() {",
  "  1 + 1",
  "  if (x) {",
  "    k()",
  "  }",
  "}",
  ""
) %>%
  rep(10)

capture.output(test_that("activated cache brings speedup on style_text() API on character vector", {
  skip_if_not_installed("R.cache")
  activate_testthat_cache()
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(text))
  second <- system.time(styler::style_text(text))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))

capture.output(test_that("activated cache brings speedup on style_text() API on character scalar", {
  skip_if_not_installed("R.cache")
  activate_testthat_cache()
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(test_that("no speedup when tranformer changes", {
  skip_if_not_installed("R.cache")
  activate_testthat_cache()
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()
  t1 <- tidyverse_style()
  first <- system.time(style_text(text, transformers = t1))
  t1$use_raw_indention <- !t1$use_raw_indention
  second <- system.time(style_text(text, transformers = t1))
  expect_false(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(
  test_that(paste0(
    "activated cache brings speedup on style_text() API on ",
    "character scalar and character vector (mixed)"
  ), {
    skip_if_not_installed("R.cache")
    activate_testthat_cache()
    on.exit(clear_testthat_cache())
    clear_testthat_cache()
    activate_testthat_cache()

    first <- system.time(styler::style_text(text))
    second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
    expect_true(first["elapsed"] / 2 > second["elapsed"])
  }))


capture.output(test_that("unactivated cache does not bring speedup", {
  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache)
  clear_testthat_cache()
  cache_deactivate()
  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_false(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(test_that("avoid deleting comments #584 (see commit messages)", {

  skip_if_not_installed("R.cache")
  on.exit(clear_testthat_cache)
  clear_testthat_cache()
  activate_testthat_cache()
  text <- c(
    "1 + 1",
    "# Comment",
    "# another",
    "NULL"
    )
  style_text(text)
  text2 <- c(
    "1 + 1",
    "# x",
    "# another",
    "NULL"
  )
  expect_equal(
    as.character(style_text(text2)),
    text2
  )
}))
