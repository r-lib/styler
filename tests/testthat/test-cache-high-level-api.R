capture.output(test_that("activated cache brings speedup on style_file() API", {
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

  activate_testthat_cache()
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(text))
  second <- system.time(styler::style_text(text))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))

capture.output(test_that("activated cache brings speedup on style_text() API on character scalar", {

  activate_testthat_cache()
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
}))


test_that("trailing line breaks are ignored for caching", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  second <- system.time(
    styler::style_text(c(paste0(text, collapse = "\n"), "\n", "\n", "\n", "\n"))
  )
  expect_true(first["elapsed"] / 2 > second["elapsed"])
  # check we only have three different expressions. Top-level, example and fun.
  cache_info <- cache_info()
  expect_equal(
    cache_info$n,
    3
  )
})

test_that("trailing line breaks are ignored for caching in one scalar", {
  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()

  first <- system.time(styler::style_text(paste0(text, collapse = "\n")))
  second <- system.time(
    styler::style_text(
      paste0(paste0(text, collapse = "\n"), "\n", "\n", "\n", "\n", collapse = "")
  ))
  expect_true(first["elapsed"] / 2 > second["elapsed"])
  # check we only have three different expressions. Top-level, example and fun.
  cache_info <- cache_info()
  expect_equal(
    cache_info$n,
    3
  )
})

capture.output(test_that("no speedup when tranformer changes", {

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
    activate_testthat_cache()
    on.exit(clear_testthat_cache())
    clear_testthat_cache()
    activate_testthat_cache()

    first <- system.time(styler::style_text(text))
    second <- system.time(styler::style_text(paste0(text, collapse = "\n")))
    expect_true(first["elapsed"] / 2 > second["elapsed"])
  }))


capture.output(test_that("unactivated cache does not bring speedup", {

  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  cache_deactivate()
  first <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  second <- system.time(styler::style_file(test_path("reference-objects/caching.R")))
  expect_false(first["elapsed"] / 2 > second["elapsed"])
}))


capture.output(test_that("avoid deleting comments #584 (see commit messages)", {

  on.exit(clear_testthat_cache())
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


capture.output(test_that("avoid removing roxygen mask (see commit messages in #584)", {

  on.exit(clear_testthat_cache())
  clear_testthat_cache()
  activate_testthat_cache()
  text <- c(
    "c(",
    " 1, 2,",
    "  x - 2",
    ")"
  )
  style_text(text)
  text2 <- c(
    "#' Stuff",
    "#'",
    "#' @examples",
    "#' c(",
    "#'   1, 2,",
    "#'   x - 2",
    "#' )",
    "#' x",
    "NULL"
  )
  expect_equal(
    as.character(style_text(text2)),
    text2
  )
}))
