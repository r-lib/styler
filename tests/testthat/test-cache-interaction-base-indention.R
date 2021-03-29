test_that("base_indention is respected in caching", {
  on.exit(clear_testthat_cache())
  text <- c("1 + 1")
  fresh_testthat_cache()
  without_indention <- style_text(text)
  fresh_testthat_cache()
  style_text(text, base_indention = 5)
  expect_equal(
    style_text(text),
    without_indention
  )
})

test_that("include_roxygen_exmples is respected in caching", {
  on.exit(clear_testthat_cache())
  text <- c("#' Roxygen", "#'", "#' @examplesIf", "#' 1+1", "1 + 1")
  fresh_testthat_cache()
  with_examples <- style_text(text)
  fresh_testthat_cache()
  style_text(text, include_roxygen_examples = FALSE)
  expect_equal(
    style_text(text, include_roxygen_examples = TRUE),
    with_examples
  )
})


test_that("expression caching when first expression does not comply", {
  on.exit(clear_testthat_cache())
  fresh_testthat_cache()
  more <- 'x<- 1
   "multi
line string"
   c(a = 3)
   another(
     "x", y = 4
   )
'
  expect_out <- c(
    "   x <- 1",
    '   "multi',
    'line string"',
    "   c(a = 3)",
    "   another(",
    '     "x",',
    "     y = 4",
    "   )"
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
  out <- style_text(more, base_indention = 4) %>%
    as.character()
  expect_equal(
    out,
    c(
      "    x <- 1",
      '    "multi',
      'line string"',
      "    c(a = 3)",
      "    another(",
      '      "x",',
      "      y = 4",
      "    )"
    )
  )
  sg <- tidyverse_style()
  # TODO caching with base indention 3
  expect_true(
    is_cached("x <- 1", sg, more_specs = cache_more_specs(TRUE, 4))
  )

  sg <- tidyverse_style()
  expect_true(
    is_cached("x <- 1", sg, more_specs = cache_more_specs(TRUE, 3))
  )
})

test_that("expression caching when last expression does not comply", {
  on.exit(clear_testthat_cache())
  fresh_testthat_cache()
  more <- '   x <- 1
   "multi
line string"
   c(a = 3)
   another(
     "x", y = 4)
'
  expect_out <- c(
    "   x <- 1",
    '   "multi',
    'line string"',
    "   c(a = 3)",
    "   another(",
    '     "x",',
    "     y = 4",
    "   )"
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
})

test_that("expression caching when middle expression does not comply", {
  on.exit(clear_testthat_cache())
  fresh_testthat_cache()
  more <- '   x <- 1
   "multi
line string"
   c(a= 3)
   another(
     "x", y = 4
  )
'
  expect_out <- c(
    "   x <- 1",
    '   "multi',
    'line string"',
    "   c(a = 3)",
    "   another(",
    '     "x",',
    "     y = 4",
    "   )"
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
  out <- style_text(more, base_indention = 3) %>%
    as.character()
  expect_equal(
    out,
    expect_out
  )
})


test_that("cache is deactivated at end of caching related testthat file", {
  expect_false(cache_is_activated())
})
