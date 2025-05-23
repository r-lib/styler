test_that("line breaks work in general", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "token_dependent_mixed",
    transformer = style_text
  ))

  expect_no_warning(test_collection("line_breaks_fun_call",
    "token_dependent_complex_strict",
    transformer = style_text
  ))
})

test_that("blank lines in function calls are removed for strict = TRUE", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "blank-strict",
    transformer = style_text
  ))

  expect_no_warning(test_collection("line_breaks_fun_call",
    "blank-non-strict",
    transformer = style_text, strict = FALSE
  ))
})


test_that("line breaks are not applied with non-strict", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "token_dependent_complex_non_strict",
    transformer = style_text, strict = FALSE
  ))
})

test_that("line breaks work with comments", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "token_dependent_comments",
    transformer = style_text
  ))
  expect_no_warning(test_collection("line_breaks_fun_call",
    "line_breaks_and_comments",
    transformer = style_text
  ))
})

test_that("line breaks work with exceptions", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "switch_ifelse",
    transformer = style_text
  ))
})

test_that("line breaks work with exceptions", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "named_arguments",
    transformer = style_text
  ))
})

test_that("line breaks work with exceptions", {
  expect_no_warning(test_collection("line_breaks_fun_call",
    "unindent",
    transformer = style_text
  ))
})
