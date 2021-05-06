test_that("activated cache brings speedup on style_file() API", {
  local_test_setup()
  skip_on_cran()
  n <- n_times_faster_with_cache(
    test_path("reference-objects/caching.R"),
    test_path("reference-objects/caching.R"),
    fun = style_file
  )
  expect_gt(n, 30)
})

text <- c(
  "#' Roxygen",
  "#' Comment",
  "#' @examples 1 + 1",
  "k <- function() {",
  "  1 + 3",
  "  if (x) {",
  "    k()",
  "  }",
  "}",
  ""
) %>%
  rep(10)

test_that("activated cache brings speedup on style_text() API on character vector", {
  skip_on_cran()
  n <- n_times_faster_with_cache(
    text, text,
    fun = style_text
  )
  expect_gt(n, 40)
})

test_that("activated cache brings speedup on style_text() API on character scalar", {
  skip_on_cran()
  text2 <- paste0(text, collapse = "\n")

  n <- n_times_faster_with_cache(
    text2, text2,
    fun = style_text
  )
  expect_gt(n, 55)
})


test_that("trailing line breaks are ignored for caching", {
  text1 <- paste0(text, collapse = "\n")
  text2 <- c(paste0(text, collapse = "\n"), "\n", "\n", "\n", "\n")
  n <- n_times_faster_with_cache(text1, text2, clear = "all but last")
  expect_equal(cache_info()$n, 3)
  skip_on_cran()
  expect_gt(n, 55)
})

test_that("trailing line breaks are ignored for caching in one scalar", {
  text1 <- paste0(text, collapse = "\n")
  text2 <- c(paste0(text, collapse = "\n"), "\n", "\n", "\n", "\n")
  n <- n_times_faster_with_cache(text1, text2, clear = "all but last")
  expect_equal(cache_info()$n, 3)
  skip_on_cran()
  expect_gt(n, 55)
})

test_that("trailing line breaks are ignored for caching in one scalar", {
  text1 <- paste0(text, collapse = "\n")
  text2 <- paste0(
    paste0(text, collapse = "\n"), "\n", "\n", "\n", "\n",
    collapse = ""
  )
  n <- n_times_faster_with_cache(text1, text2, clear = "all but last")
  expect_equal(cache_info()$n, 3)
  skip_on_cran()
  expect_gt(n, 55)
})

test_that("speedup higher when cached roxygen example code is multiple expressions", {
  skip_on_cran()

  text_long <- c(
    "#' Roxygen",
    "#' Comment",
    "#' @examples",
    "#' call(1 + 1, 33)",
    "#' if (x > 4)",
    "#' bb = 3",
    "#' call(x,y=2)",
    "k <- function() {",
    "  1 + 1",
    "  if (x) {",
    "    k()",
    "  }",
    "}",
    ""
  )
  text_long_styled <- style_text(text_long)
  text_long_styled_changed <- text_long_styled
  text_long_styled_changed[14] <- "    }"
  speedup_multiple_roygen_example <- n_times_faster_with_cache(
    text_long_styled, text_long_styled_changed
  )
  text_short_styled <- text_long_styled[-c(5:8)]
  text_short_styled_changed <- text_short_styled
  text_short_styled_changed[10] <- "   }"
  speedup_many_roygen_examples <- n_times_faster_with_cache(
    text_short_styled, text_short_styled_changed
  )
  # the speed gain for longer expression is 1.1x higher
  expect_true(
    speedup_multiple_roygen_example / speedup_many_roygen_examples > 1.05
  )
})



test_that("no speedup when tranformer changes", {
  skip_on_cran()
  expect_true(TRUE)
  local_test_setup()
  t1 <- tidyverse_style()
  first <- system.time(style_text(text, transformers = t1))
  t1 <- tidyverse_style(indent_by = 4)
  second <- system.time(style_text(text, transformers = t1))
  expect_false(first["elapsed"] / 1.3 > second["elapsed"])
})


test_that("unactivated cache does not bring speedup", {
  skip_on_cran
  local_test_setup()
  first <- system.time(style_file(test_path("reference-objects/caching.R")))
  second <- system.time(style_file(test_path("reference-objects/caching.R")))
  expect_false(first["elapsed"] / 4 > second["elapsed"])
})

test_that("avoid deleting comments #584 (see commit messages)", {
  local_test_setup()
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
  expect_equal(as.character(style_text(text2)), text2)
})

test_that("avoid removing roxygen mask (see commit messages in #584)", {
  local_test_setup()
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
  expect_equal(as.character(style_text(text2)), text2)
})

test_that("partial caching of multiple expressions on one line works", {
  local_test_setup()
  text <- "1"
  style_text(text)
  text2 <- "1 # comment"
  styled <- style_text(text2)
  expect_equal(
    as.character(styled),
    text2
  )

  style_text("mtcars")
  style_text(c("mtcars %>%", "f()"))
  final_text <- c("mtcars %>%", "  f() #")
  expect_equal(as.character(style_text(final_text)), final_text)
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
