test_that("extra line breaks between conditional statements are removed", {
  expect_no_warning(
    test_collection("line_breaks_top_level_exprs", "conditionals", transformer = style_text)
  )
})

test_that("extra line breaks between function and definitions and calls are removed", {
  expect_no_warning(
    test_collection("line_breaks_top_level_exprs", "function_defs_and_calls", transformer = style_text)
  )
})

test_that("extra line breaks between piped chains are removed", {
  expect_no_warning(
    test_collection("line_breaks_top_level_exprs", "piped_chains", transformer = style_text)
  )
})

test_that("extra line breaks between braced expressions are removed", {
  expect_no_warning(
    test_collection("line_breaks_top_level_exprs", "braces", transformer = style_text)
  )
})

test_that("extra line breaks are not removed in non-strict mode", {
  expect_no_warning(
    test_collection("line_breaks_top_level_exprs", "non_strict", transformer = style_text, strict = FALSE)
  )
})
