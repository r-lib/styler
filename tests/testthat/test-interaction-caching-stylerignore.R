test_that("caching works with stylerignore for multi-token lines when partly cached before", {
  text1 <- "1 + 1"
  activate_testthat_cache()
  cache_clear(ask = FALSE)
  activate_testthat_cache()
  expect_equal(
    as.character(style_text(text1)),
    text1
  )

  text2 <- c(
    "# styler: off",
    "1 + 1",
    "# styler: on",
    "# a comment"
  )
  expect_equal(
    as.character(style_text(text2)),
    text2
  )
})

test_that("caching works with stylerignore for multi-token lines", {
  on.exit(cache_deactivate())
  text3 <- c(
    "# styler: off",
    "1 + 1 #comment2",
    "# styler: on",
    "#a comment"
  )
  text3_correct <- c(
    "# styler: off",
    "1 + 1 #comment2",
    "# styler: on",
    "# a comment"
  )

  expect_equal(
    as.character(style_text(text3)),
    text3_correct
  )

  expect_equal(
    as.character(style_text(text3_correct)),
    text3_correct
  )

  text4 <- c(
    "# styler: off",
    "1 +1",
    "x(x)",
    "# styler: on",
    "# a comment"
  )

  expect_equal(
    as.character(style_text(text4)),
    text4
  )
})
