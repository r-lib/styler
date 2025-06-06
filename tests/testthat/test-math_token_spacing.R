test_that("invalid tokens return error", {
  expect_error(test_collection(
    "math_token_spacing", "non_strict_math_spacing_all",
    transformer = style_text,
    style = tidyverse_style,
    scope = "spaces",
    math_token_spacing = specify_math_token_spacing("hdk"),
    strict = FALSE
  ), "lookup")
})

test_that("non-strict default: spacing around all", {
  expect_no_warning(test_collection(
    "math_token_spacing", "non_strict_math_spacing_all",
    transformer = style_text,
    style = tidyverse_style,
    scope = "spaces",
    math_token_spacing = specify_math_token_spacing(),
    strict = FALSE
  ))
})

test_that("strict default: spacing around all", {
  expect_no_warning(test_collection(
    "math_token_spacing", "strict_math_spacing_all",
    transformer = style_text,
    style = tidyverse_style,
    scope = "spaces",
    math_token_spacing = tidyverse_math_token_spacing(),
    strict = TRUE
  ))
})

test_that("strict no space around +", {
  expect_no_warning(test_collection(
    "math_token_spacing", "strict_math_spacing_zero_plus",
    transformer = style_text,
    style = tidyverse_style,
    scope = "spaces",
    math_token_spacing = specify_math_token_spacing(zero = "'+'")
  ))
})

test_that("strict no space around all but ^", {
  expect_no_warning(test_collection(
    "math_token_spacing", "strict_math_spacing_zero_all_but_power",
    transformer = style_text,
    style = tidyverse_style,
    scope = "spaces",
    math_token_spacing = specify_math_token_spacing(zero = c(
      "'+'", "'-'", "'/'", "'*'"
    ))
  ))
})
