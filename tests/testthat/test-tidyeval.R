test_that("no spaces within bang-bang operator !!!", {
  expect_no_warning(test_collection("tidyeval", "bang_bang",
    transformer = style_text
  ))
})

test_that(":= has correct spacing", {
  expect_no_warning(test_collection("tidyeval", "setting_var",
    transformer = style_text
  ))
})

test_that("Space before comma if preceding token is EQ_SUB", {
  expect_no_warning(test_collection("tidyeval", "eq_sub",
    transformer = style_text
  ))
})
