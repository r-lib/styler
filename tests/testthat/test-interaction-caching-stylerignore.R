test_that("caching works with stylerignore for multi-token lines when partly cached before", {
  local_test_setup(cache = TRUE)
  text1 <- "1 + 1"
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
  local_test_setup(cache = TRUE)
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

test_that("caching works ", {
  local_test_setup(cache = TRUE)
  text1 <- "1 + 1"
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

# when a top-level expression is cached, it means it is already complying to
# the style.
# Since top-level comments are not cached, the expression in the stylerignore
# sequence will be in a different block if cached and not be senth though
# apply_stylerignore.

# if the stylerignore tag is top-level
test_that("caching works for top-level expressions", {
  local_test_setup(cache = TRUE)
  text1 <- "1 + 1"
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

# if the stylerignore tag is not top-level
# since we only cache top-level expressions, the whole expression is either
# cached or not, depending on whether it is complying to the style guide.
test_that("caching works for non-top-level expressions", {
  local_test_setup(cache = TRUE)
  text1 <- "1 + 1"
  expect_equal(
    as.character(style_text(text1)),
    text1
  )

  text2 <- c(
    "cal8(",
    "  # styler: off",
    "  1 + 1,",
    "  # styler: on",
    ")",
    "# comment"
  )
  expect_equal(
    as.character(style_text(text2)),
    text2
  )
})

test_that("does not cache stylerignore sequences", {
  local_test_setup(cache = TRUE)
  text <- c(
    "1+1# styler: off"
  )
  style_text(text)
  expect_false(
    is_cached("1+1", tidyverse_style(), more_specs = cache_more_specs_default())
  )
  local_test_setup(cache = TRUE)
  text <- c(
    "# styler: off",
    "1+1"
  )
  style_text(text)
  expect_false(
    is_cached(
      "1+1",
      tidyverse_style(),
      more_specs = cache_more_specs_default()
    )
  )
})

test_that("indention preserved in stylerignore when caching activated", {
  local_test_setup(cache = TRUE)
  text6 <- c(
    "# styler: off",
    "1 + 1",
    "    x(5)",
    "# styler: on",
    "# a comment"
  )
  expect_equal(
    as.character(style_text(text6)),
    text6
  )
})

test_that("changing ignore markers invalidates cache", {
  opts <- list(
    list(styler.ignore_stop = "noqua: stop", n = 1),
    list(styler.ignore_start = "noqua: start", n = 3)
  )
  purrr::walk(opts, function(opt) {
    local_test_setup(cache = TRUE)
    text7 <- c(
      "# styler: off",
      "1 + 1",
      "# styler: on"
    )
    style_text(text7)
    rlang::exec(withr::local_options, !!!opt[-length(opt)])
    style_text(text7)
    expect_equal(cache_info(format = "tabular")$n, opt[["n"]])
  })
})



test_that("all expressions within a stylerignore sequence (whether cached or not) are put in the same block (low-level)", {
  transformers <- tidyverse_style()
  specs <- transformers$more_specs_style_guide
  full <- c(
    "# styler: off",
    "a",
    "flush(",
    "1",
    ")",
    "# styler: on"
  )
  without_ignore <- full[c(-1L, -length(full))]
  local_test_setup(cache = TRUE)

  expect_true(all(compute_parse_data_nested(without_ignore, transformers, specs)$block == 1L))

  cache_by_expression("a", transformers, more_specs = NULL)
  is_cached("a", transformers, more_specs = NULL)
  cache_by_expression("flush(\n  1\n)", transformers, more_specs = NULL)
  cache_by_expression(c("a", "flush(", "  1", ")"), transformers, more_specs = NULL)

  expect_true(all(compute_parse_data_nested(full)$block == 1L))
})


test_that("all expressions within a stylerignore sequence (whether cached or not) are put in the same block (high-level)", {
  full <- c(
    "# styler: off",
    "a",
    "flush(",
    "1",
    ")",
    "# styler: on"
  )
  without_ignore <- full[c(-1L, -length(full))]
  local_test_setup(cache = TRUE)

  expect_identical(as.character(style_text(without_ignore)), c("a", "flush(", "  1", ")"))
  expect_identical(as.character(style_text(full)), full)
})

test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
