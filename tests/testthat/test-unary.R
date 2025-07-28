test_that("no spaces before unary operator", {
  expect_no_warning(test_collection("unary_spacing",
    "unary_simple",
    transformer = style_text
  ))

  expect_no_warning(test_collection("unary_spacing",
    "unary_complex",
    transformer = style_text
  ))

  expect_no_warning(test_collection("unary_spacing",
    "unary_indention",
    transformer = style_text
  ))
})
