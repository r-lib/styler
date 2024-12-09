test_that("edge cases work", {
  expect_no_warning(test_collection("indention_fun_calls",
    transformer = style_text, strict = FALSE
  ))
})
