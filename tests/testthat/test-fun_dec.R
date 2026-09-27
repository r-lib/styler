test_that("reindent function declaration", {
  expect_no_warning(test_collection("fun_dec", "fun_dec_scope_spaces",
    transformer = style_text, scope = "spaces"
  ))

  expect_no_warning(test_collection("fun_dec", "line_break_fun_dec",
    transformer = style_text
  ))
})

test_that("function declaration header is indented by indent_by", {
  expect_no_warning(test_collection("fun_dec", "fun_dec_indent_by",
    transformer = style_text, indent_by = 4
  ))
})
