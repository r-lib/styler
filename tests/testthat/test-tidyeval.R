context("tidyeval")

test_that("no spaces within bang-bang operator !!!", {
  expect_warning(test_collection("tidyeval", "bang_bang",
    transformer = style_text
  ), NA)
})

test_that(":= has correct spacing", {
  expect_warning(test_collection("tidyeval", "setting_var",
    transformer = style_text
  ), NA)
})

test_that("Space before comma if preceding token is EQ_SUB", {
  expect_warning(test_collection("tidyeval", "eq_sub",
    transformer = style_text
  ), NA)
})
