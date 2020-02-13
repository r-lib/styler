test_that("reindent function declaration", {
  expect_warning(test_collection("fun_dec", "fun_dec_scope_spaces",
    transformer = style_text, scope = "spaces"
  ), NA)

  expect_warning(test_collection("fun_dec", "line_break_fun_dec",
    transformer = style_text
  ), NA)
})
